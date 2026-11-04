// Port of app/src/net/sexpr.ts. Keep the two in step.
//
// S-expression codec for the Racket server protocol.
// Handles the subset Racket `write` produces for our messages:
// symbols, strings, numbers, #t/#f, proper lists, dotted pairs,
// prefab structs #s(card ...) and immutable hashes #hash((k . v) ...).
package dev.synth.core

import java.math.BigDecimal

sealed interface SExpr

// Doubles, as in the JS client: 1 and 1.0 are the same number.
data class SNum(val value: Double) : SExpr {
    constructor(value: Int) : this(value.toDouble())
}

data class SStr(val value: String) : SExpr
data class SBool(val value: Boolean) : SExpr
data class Sym(val name: String) : SExpr
data class SList(val items: List<SExpr>) : SExpr

// `Pair` in the TS; renamed so it does not shadow kotlin.Pair in this package.
data class SPair(val car: SExpr, val cdr: SExpr) : SExpr
data class Prefab(val name: String, val fields: List<SExpr>) : SExpr
data class SHash(val entries: List<Pair<SExpr, SExpr>>) : SExpr

fun sym(name: String): Sym = Sym(name)

fun isSym(value: SExpr?, name: String? = null): Boolean =
    value is Sym && (name == null || value.name == name)

private sealed interface Token {
    data object Open : Token
    data object Close : Token
    data object Dot : Token
    data object PrefabOpen : Token
    data object HashOpen : Token
    data class Atom(val value: SExpr) : Token
}

// JS `\s`, which is not quite Char.isWhitespace.
internal fun isJsSpace(ch: Char): Boolean = when (ch) {
    '\t', '\n', '\u000B', '\u000C', '\r', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', '　', '﻿' -> true
    else -> ch in ' '..' '
}

private val INTEGER = Regex("[+-]?[0-9]+")
private val DECIMAL = Regex("[+-]?[0-9]*\\.[0-9]+")

private fun tokenize(input: String): List<Token> {
    val tokens = mutableListOf<Token>()
    var i = 0

    fun isDelimiter(ch: Char) = ch == '(' || ch == ')' || isJsSpace(ch)

    while (i < input.length) {
        val ch = input[i]
        if (isJsSpace(ch)) {
            i++
        } else if (ch == '(') {
            tokens.add(Token.Open)
            i++
        } else if (ch == ')') {
            tokens.add(Token.Close)
            i++
        } else if (ch == '"') {
            val value = StringBuilder()
            i++
            while (i < input.length && input[i] != '"') {
                if (input[i] == '\\' && i + 1 < input.length) {
                    val escaped = input[i + 1]
                    value.append(
                        when (escaped) {
                            'n' -> '\n'
                            't' -> '\t'
                            else -> escaped
                        }
                    )
                    i += 2
                } else {
                    value.append(input[i])
                    i++
                }
            }
            i++ // closing quote
            tokens.add(Token.Atom(SStr(value.toString())))
        } else if (ch == '#') {
            if (input.startsWith("#s(", i)) {
                tokens.add(Token.PrefabOpen)
                i += 3
            } else if (input.startsWith("#hash(", i)) {
                tokens.add(Token.HashOpen)
                i += 6
            } else if (input.startsWith("#t", i) && (i + 2 >= input.length || isDelimiter(input[i + 2]))) {
                tokens.add(Token.Atom(SBool(true)))
                i += 2
            } else if (input.startsWith("#f", i) && (i + 2 >= input.length || isDelimiter(input[i + 2]))) {
                tokens.add(Token.Atom(SBool(false)))
                i += 2
            } else if (input.startsWith("#true", i)) {
                tokens.add(Token.Atom(SBool(true)))
                i += 5
            } else if (input.startsWith("#false", i)) {
                tokens.add(Token.Atom(SBool(false)))
                i += 6
            } else {
                val end = minOf(i + 10, input.length)
                throw IllegalArgumentException("sexpr: unsupported reader syntax at \"${input.substring(i, end)}\"")
            }
        } else {
            val text = StringBuilder()
            while (i < input.length && !isDelimiter(input[i])) {
                text.append(input[i])
                i++
            }
            val word = text.toString()
            if (word == ".") {
                tokens.add(Token.Dot)
            } else if (INTEGER.matches(word) || DECIMAL.matches(word)) {
                tokens.add(Token.Atom(SNum(word.toDouble())))
            } else {
                tokens.add(Token.Atom(Sym(word)))
            }
        }
    }

    return tokens
}

private class Parser(private val tokens: List<Token>) {
    private var pos = 0

    fun parse(): SExpr {
        val result = parseExpr()
        if (pos < tokens.size) {
            throw IllegalArgumentException("sexpr: trailing tokens after expression")
        }
        return result
    }

    private fun peek(): Token? = tokens.getOrNull(pos)

    private fun next(): Token {
        val token = tokens.getOrNull(pos) ?: throw IllegalArgumentException("sexpr: unexpected end of input")
        pos++
        return token
    }

    private fun parseExpr(): SExpr = when (val token = next()) {
        is Token.Atom -> token.value
        Token.Open -> parseListBody()
        Token.PrefabOpen -> {
            val elements = parseElementsUntilClose()
            val name = elements.getOrNull(0) as? Sym
                ?: throw IllegalArgumentException("sexpr: prefab struct without symbol name")
            Prefab(name.name, elements.drop(1))
        }
        Token.HashOpen -> SHash(
            parseElementsUntilClose().map { entry ->
                if (entry is SPair) Pair(entry.car, entry.cdr)
                else throw IllegalArgumentException("sexpr: hash entry is not a pair")
            }
        )
        else -> throw IllegalArgumentException("sexpr: unexpected token \"$token\"")
    }

    // After an "(", parse elements handling an optional dotted tail.
    private fun parseListBody(): SExpr {
        val elements = mutableListOf<SExpr>()
        while (true) {
            val token = peek() ?: throw IllegalArgumentException("sexpr: unterminated list")
            if (token == Token.Close) {
                pos++
                return SList(elements)
            }
            if (token == Token.Dot) {
                pos++
                val tail = parseExpr()
                if (next() != Token.Close) throw IllegalArgumentException("sexpr: expected ) after dotted tail")
                // (a b . (c d)) is the proper list (a b c d)
                if (tail is SList) return SList(elements + tail.items)
                if (elements.isEmpty()) throw IllegalArgumentException("sexpr: dotted tail without a head")
                // (a b . c) — chain into nested pairs
                var result: SExpr = tail
                for (i in elements.size - 1 downTo 1) {
                    result = SPair(elements[i], result)
                }
                return SPair(elements[0], result)
            }
            elements.add(parseExpr())
        }
    }

    private fun parseElementsUntilClose(): List<SExpr> {
        val elements = mutableListOf<SExpr>()
        while (true) {
            val token = peek() ?: throw IllegalArgumentException("sexpr: unterminated form")
            if (token == Token.Close) {
                pos++
                return elements
            }
            elements.add(parseExpr())
        }
    }
}

fun parseSExpr(input: String): SExpr = Parser(tokenize(input)).parse()

fun writeSExpr(expr: SExpr): String = when (expr) {
    is SNum -> BigDecimal(expr.value.toString()).stripTrailingZeros().toPlainString()
    is SBool -> if (expr.value) "#t" else "#f"
    is SStr -> "\"" + expr.value.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    is Sym -> expr.name
    is SList -> expr.items.joinToString(" ", "(", ")") { writeSExpr(it) }
    is SPair -> "(${writeSExpr(expr.car)} . ${writeSExpr(expr.cdr)})"
    is Prefab -> (listOf(expr.name) + expr.fields.map { writeSExpr(it) }).joinToString(" ", "#s(", ")")
    is SHash -> expr.entries.joinToString(" ", "#hash(", ")") { (k, v) -> "(${writeSExpr(k)} . ${writeSExpr(v)})" }
}

// --- helpers for picking protocol messages apart ---

// Looks up a key in an association list whose entries may be pairs
// ((k . v)) or lists ((k v ...)) depending on how Racket printed them.
fun assocValue(alist: SExpr?, key: String): SExpr? {
    if (alist !is SList) return null
    for (entry in alist.items) {
        if (entry is SPair && isSym(entry.car, key)) return entry.cdr
        if (entry is SList && entry.items.isNotEmpty() && isSym(entry.items[0], key)) {
            return SList(entry.items.drop(1))
        }
    }
    return null
}
