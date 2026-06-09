// S-expression codec for the Racket server protocol.
// Handles the subset Racket `write` produces for our messages:
// symbols, strings, numbers, #t/#f, proper lists, dotted pairs,
// prefab structs #s(card ...) and immutable hashes #hash((k . v) ...).

export class Sym {
  constructor(readonly name: string) {}
}

export class Pair {
  constructor(readonly car: SExpr, readonly cdr: SExpr) {}
}

export class Prefab {
  constructor(readonly name: string, readonly fields: SExpr[]) {}
}

export class SHash {
  constructor(readonly entries: Array<[SExpr, SExpr]>) {}
}

export type SExpr = number | string | boolean | Sym | SExpr[] | Pair | Prefab | SHash;

export const sym = (name: string): Sym => new Sym(name);

export const isSym = (value: SExpr, name?: string): value is Sym =>
  value instanceof Sym && (name === undefined || value.name === name);

type Token =
  | { kind: "open" }
  | { kind: "close" }
  | { kind: "dot" }
  | { kind: "prefab-open" }
  | { kind: "hash-open" }
  | { kind: "atom"; value: number | string | boolean | Sym };

function tokenize(input: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;

  const isDelimiter = (ch: string) => ch === "(" || ch === ")" || /\s/.test(ch);

  while (i < input.length) {
    const ch = input[i];
    if (/\s/.test(ch)) {
      i++;
    } else if (ch === "(") {
      tokens.push({ kind: "open" });
      i++;
    } else if (ch === ")") {
      tokens.push({ kind: "close" });
      i++;
    } else if (ch === '"') {
      let value = "";
      i++;
      while (i < input.length && input[i] !== '"') {
        if (input[i] === "\\" && i + 1 < input.length) {
          const escaped = input[i + 1];
          value += escaped === "n" ? "\n" : escaped === "t" ? "\t" : escaped;
          i += 2;
        } else {
          value += input[i];
          i++;
        }
      }
      i++; // closing quote
      tokens.push({ kind: "atom", value });
    } else if (ch === "#") {
      if (input.startsWith("#s(", i)) {
        tokens.push({ kind: "prefab-open" });
        i += 3;
      } else if (input.startsWith("#hash(", i)) {
        tokens.push({ kind: "hash-open" });
        i += 6;
      } else if (input.startsWith("#t", i) && (i + 2 >= input.length || isDelimiter(input[i + 2]))) {
        tokens.push({ kind: "atom", value: true });
        i += 2;
      } else if (input.startsWith("#f", i) && (i + 2 >= input.length || isDelimiter(input[i + 2]))) {
        tokens.push({ kind: "atom", value: false });
        i += 2;
      } else if (input.startsWith("#true", i)) {
        tokens.push({ kind: "atom", value: true });
        i += 5;
      } else if (input.startsWith("#false", i)) {
        tokens.push({ kind: "atom", value: false });
        i += 6;
      } else {
        throw new Error(`sexpr: unsupported reader syntax at "${input.slice(i, i + 10)}"`);
      }
    } else {
      let text = "";
      while (i < input.length && !isDelimiter(input[i])) {
        text += input[i];
        i++;
      }
      if (text === ".") {
        tokens.push({ kind: "dot" });
      } else if (/^[+-]?\d+$/.test(text)) {
        tokens.push({ kind: "atom", value: parseInt(text, 10) });
      } else if (/^[+-]?\d*\.\d+$/.test(text)) {
        tokens.push({ kind: "atom", value: parseFloat(text) });
      } else {
        tokens.push({ kind: "atom", value: new Sym(text) });
      }
    }
  }

  return tokens;
}

class Parser {
  private pos = 0;

  constructor(private tokens: Token[]) {}

  parse(): SExpr {
    const result = this.parseExpr();
    if (this.pos < this.tokens.length) {
      throw new Error("sexpr: trailing tokens after expression");
    }
    return result;
  }

  private peek(): Token | undefined {
    return this.tokens[this.pos];
  }

  private next(): Token {
    const token = this.tokens[this.pos];
    if (!token) throw new Error("sexpr: unexpected end of input");
    this.pos++;
    return token;
  }

  private parseExpr(): SExpr {
    const token = this.next();
    switch (token.kind) {
      case "atom":
        return token.value;
      case "open":
        return this.parseListBody();
      case "prefab-open": {
        const elements = this.parseElementsUntilClose();
        const name = elements[0];
        if (!(name instanceof Sym)) throw new Error("sexpr: prefab struct without symbol name");
        return new Prefab(name.name, elements.slice(1));
      }
      case "hash-open": {
        const elements = this.parseElementsUntilClose();
        const entries = elements.map((entry): [SExpr, SExpr] => {
          if (entry instanceof Pair) return [entry.car, entry.cdr];
          throw new Error("sexpr: hash entry is not a pair");
        });
        return new SHash(entries);
      }
      default:
        throw new Error(`sexpr: unexpected token "${token.kind}"`);
    }
  }

  // After an "(", parse elements handling an optional dotted tail.
  private parseListBody(): SExpr {
    const elements: SExpr[] = [];
    for (;;) {
      const token = this.peek();
      if (!token) throw new Error("sexpr: unterminated list");
      if (token.kind === "close") {
        this.pos++;
        return elements;
      }
      if (token.kind === "dot") {
        this.pos++;
        const tail = this.parseExpr();
        const close = this.next();
        if (close.kind !== "close") throw new Error("sexpr: expected ) after dotted tail");
        // (a b . (c d)) is the proper list (a b c d)
        if (Array.isArray(tail)) return [...elements, ...tail];
        // (a b . c) — chain into nested pairs
        let result: SExpr = tail;
        for (let i = elements.length - 1; i >= 1; i--) {
          result = new Pair(elements[i], result);
        }
        return new Pair(elements[0], result);
      }
      elements.push(this.parseExpr());
    }
  }

  private parseElementsUntilClose(): SExpr[] {
    const elements: SExpr[] = [];
    for (;;) {
      const token = this.peek();
      if (!token) throw new Error("sexpr: unterminated form");
      if (token.kind === "close") {
        this.pos++;
        return elements;
      }
      elements.push(this.parseExpr());
    }
  }
}

export function parseSExpr(input: string): SExpr {
  return new Parser(tokenize(input)).parse();
}

export function writeSExpr(expr: SExpr): string {
  if (typeof expr === "number") return expr.toString();
  if (typeof expr === "boolean") return expr ? "#t" : "#f";
  if (typeof expr === "string") return `"${expr.replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`;
  if (expr instanceof Sym) return expr.name;
  if (Array.isArray(expr)) return `(${expr.map(writeSExpr).join(" ")})`;
  if (expr instanceof Pair) return `(${writeSExpr(expr.car)} . ${writeSExpr(expr.cdr)})`;
  if (expr instanceof Prefab) {
    return `#s(${[expr.name, ...expr.fields.map(writeSExpr)].join(" ")})`;
  }
  return `#hash(${expr.entries.map(([k, v]) => `(${writeSExpr(k)} . ${writeSExpr(v)})`).join(" ")})`;
}

// --- helpers for picking protocol messages apart ---

// Looks up a key in an association list whose entries may be pairs
// ((k . v)) or lists ((k v ...)) depending on how Racket printed them.
export function assocValue(alist: SExpr, key: string): SExpr | undefined {
  if (!Array.isArray(alist)) return undefined;
  for (const entry of alist) {
    if (entry instanceof Pair && isSym(entry.car, key)) return entry.cdr;
    if (Array.isArray(entry) && entry.length > 0 && isSym(entry[0], key)) return entry.slice(1);
  }
  return undefined;
}
