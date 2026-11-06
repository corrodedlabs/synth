package dev.synth.app

import android.content.ActivityNotFoundException
import android.content.Context
import android.content.Intent
import android.net.Uri
import android.util.Log
import androidx.browser.customtabs.CustomTabsClient
import androidx.browser.customtabs.CustomTabsIntent

/** The web game: the same tables, for phones and browsers the app can't serve. */
const val WEB_URL = "https://respawnfi.com"

/** The web game's link to a table, as invites share it. */
fun webGameLink(room: String): String = "$WEB_URL/?join=" + Uri.encode(room)

/**
 * Opens the web game in a Custom Tab, at [room] when an invite asked for one. The tab goes to a browser
 * by name: respawnfi.com links open this app, so a link sent to no one in particular could come straight
 * back here.
 */
fun openWebGame(context: Context, room: String?) {
    val url = Uri.parse(if (room != null) webGameLink(room) else WEB_URL)
    val browser = browserPackage(context)
    if (browser == null) {
        Log.w("synth", "no browser to open $url")
        return
    }
    Log.i("synth", "opening $url in $browser")
    val tab = CustomTabsIntent.Builder().setShowTitle(true).build()
    tab.intent.setPackage(browser)
    try {
        tab.launchUrl(context, url)
    } catch (_: ActivityNotFoundException) {
        // A browser without Custom Tabs still opens the page.
        context.startActivity(Intent(Intent.ACTION_VIEW, url).setPackage(browser))
    }
}

/** The browser to use: one with Custom Tabs, the default first, else any that opens web pages. */
private fun browserPackage(context: Context): String? {
    CustomTabsClient.getPackageName(context, null)?.let { return it }
    // Any site but respawnfi.com, so this app isn't among the answers.
    val probe = Intent(Intent.ACTION_VIEW, Uri.parse("https://example.com")).addCategory(Intent.CATEGORY_BROWSABLE)
    return context.packageManager.queryIntentActivities(probe, 0)
        .map { it.activityInfo.packageName }
        .firstOrNull { it != context.packageName }
}
