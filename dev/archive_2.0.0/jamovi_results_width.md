# The jamovi results iframe: why tabxplor tables are cut, and how to size them properly

Findings from reading the **installed jamovi 2.7.36** client bundle (`/app/lib/jamovi/client/assets/`, flatpak `org.jamovi.jamovi`, Chromium 114 / Electron 25) and the 2.6.44 live capture in `dev/jamovi/dev_console_live_capture/`. Every rule quoted below was read out of the shipped files, not inferred.

**One-line answer:** nothing in jamovi caps our width — jamovi's own stylesheet pins the *Html result element* at `width:500px`, our scroll box overflows that pinned box, and an overflowing child contributes nothing to the width the iframe reports. So the panel is sized as if the table were 500 px wide, and everything past ~620 px is clipped by the iframe edge. A visible `Image` "fixed" it only because an image is a *definite-width, in-flow* element and therefore does get counted.

---

## 1. How a result gets its width (the whole chain)

Four facts, each verbatim from the shipped bundle.

**(a) The results iframe reports its own content width to the app** — `resultsview-*.js`:

```js
_reallyNotifyResize() {
  let e = this.$results[0].getBoundingClientRect(), t = e.width + 40, n = e.height;
  this.mainWindow.postMessage({ type: "sizeChanged", data: { width: t, height: n, ... } }, "*")
}
```

`$results` is `<div id="results">`, and a resize detector (`strategy:"scroll"`) re-fires this on any layout change.

**(b) The app obeys it, with a floor and NO ceiling** — `main-*.js`:

```js
case "sizeChanged":
  let u = r.height, f = r.width;
  u < 20 && (u = 20);
  f < 620 && (f = 620);            // ← the only clamp: a 620 px MINIMUM
  o.width(f), o.height(u), d.width(f), d.height(u);
```

The iframe is `scrolling="no"`, so whatever exceeds that width is **clipped, not scrollable**. The results panel itself is `overflow:scroll`, so a wide iframe scrolls the panel horizontally — that is jamovi's native behaviour for wide content, not a failure mode.

**(c) `#results` hugs its content** — `resultsview-*.css`:

```css
#results { display:inline-block; padding-inline-start:12px; padding-inline-end:12px; box-sizing:border-box }
.jmv-results-item { display:inline-block; margin-inline-end:24px; width:max-content; ... }
```

⚠ Note `.jmv-results-item{width:max-content}` — **jamovi 2.7 already intends result items to hug their content.**

**(d) …except an Html result, pinned to 500 px** — same stylesheet, **one rule later** (byte 170254 vs 169891, so it wins on order at equal specificity):

```css
.jmv-results-html { width:500px }
.jmv-results-html .content { padding-bottom:12px }
```

## 2. Why the table is cut

Today's chain for a tabxplor result:

```text
#results            inline-block, shrink-to-fit          ← what gets measured
└ .jmv-results-item.jmv-results-html   width:500px       ← ⛔ definite width, stops here
   └ .content                          block, 500px
      └ .tx-scrollbox   width:max-content; max-width:1600px; overflow-x:auto
         └ <table>      e.g. 1400 px                     ← overflows, contributes NOTHING
```

An element with a **definite** width contributes exactly that width to its ancestors' intrinsic sizing, and an overflowing descendant contributes nothing. So:

- reported width = 500 (html item) + 24 (its margin) + 24 (`#results` padding) + 40 = **588 px**;
- 588 < 620 → the app clamps to the floor and sets the iframe to **exactly 620 px**;
- the `.tx-scrollbox` is laid out at its true width (up to the 1600 px cap) and is **clipped by the iframe at 620 px**;
- its horizontal scrollbar sits at the box's right edge — i.e. at x ≈ 1600 — so it is off-screen too.

That is precisely the reported symptom: *"cut at the right before the scroll box appears, and the scroll box itself is cut when it appears."*

**Falsifiable prediction:** every `jmvtab` / `jmvtabreg` result is currently **exactly 620 px wide**, whatever the table. Verify in F10 DevTools with `document.querySelector('iframe.analysis').style.width`, or in the results iframe with `document.getElementById('results').getBoundingClientRect()`.

### Why a visible Image "sets the width right"

An `Image` result renders as a `<div>` with an **explicit pixel width** from the `.r.yaml` (`width: 1080`). It is in-flow and definite, so it *does* contribute: `#results` becomes ~1080 wide, the iframe ~1148, and the table beside it is no longer clipped. The 2.6.44 capture shows exactly this — the tabxplor analysis iframe is `width: 1168px` while its two neighbours are at the 644 px floor.

The cost is that it reserves vertical space — a rendering Image is not a zero-height element, whatever `height:` says. So the Image is not a width mechanism worth keeping; it is a side effect, and the state carriers stay `visible:false`.

⚠ Do not re-read that as "a visible carrier breaks the render". `Image$asProtoBuf()` marks a state-holding image that wrote no file as `ANALYSIS_RENDERING` **whatever `visible` says** — the branch never reads it. What actually dropped the state was `clearWith` defaulting to `"*"`, fixed in Phase 22g-viii; `visible:false` is kept for the vertical space alone.

## 3. The fix: un-pin the Html element

One declaration, in the `<style>` block `tab_render_scrollbox()` already injects (inline `<style>` inside `content` is a documented-working channel, guide §7.2):

```css
.jmv-results-html { width: max-content; }
```

This does not fight the framework — it **restores `.jmv-results-item`'s own `width:max-content`**, which jamovi's stylesheet sets one rule earlier and then overrides for Html elements only. Consequences:

- `.content` (a block) now resolves against a max-content parent, so the chain's intrinsic width becomes the scroll box's;
- `.tx-scrollbox{width:max-content}` makes min-content == max-content, so `#results`' shrink-to-fit lands on the true width **in one pass** — no oscillation with the iframe-resize feedback loop;
- a `max-width` on the scroll box still clamps that contribution, so the cap keeps working — it just becomes real instead of invisible;
- a table narrower than ~580 px reports below the 620 px floor and shows the same white margin it does today (the floor is the app's, not ours).

Scope: our analyses each own their iframe (one per analysis — confirmed in the capture: `iframe.analysis[data-id]`), and each declares exactly one `Html` element, so an unscoped rule cannot reach any other module. Chromium 114 supports `:has()` if a narrower selector is ever wanted; it is not needed.

### ⚠ The one thing that must go with it

Un-pinning means **prose starts driving the width**. Three fragments are set outside the scroll box and have no width constraint:

- `export_status_html()` (`R/jmvtab-export.R`) — "Saved to: <long path>";
- `.hint()` (`R/jmvtabreg.b.R`) — a ~200-character sentence;
- `.compare_hint()` — the staged-comparison banner.

A normally-wrapping block's max-content is its **whole text on one line**, so `.hint()` alone would report ≈1300 px with no table on screen. Each must carry a `max-width` (a `.tx-note` class in the same injected `<style>`), or the fix trades one defect for another.

Two consistency items in the same family, worth doing at the same time:

- `.tabxplor-caption` (`R/tab-css.R`) claims in its comment to wrap at the table's width; under a max-content ancestor it does not — it drives the width. The package's own idiom fixes it: `width:0;min-width:100%`, exactly as `.tx-foot` already does.
- the `<style>` is emitted by `tab_render_scrollbox()`, which is **not** called on the hint-only paths. If prose needs the rule, the style block should move into one small `jmv_results_html(...)` assembler that every `setContent()` call goes through — one place, no second layer.

## 4. The one real decision: what to do past the cap

The app imposes no maximum, so this is entirely our choice. There is no way for the module to learn the results panel's width: the iframe's own width *is* what we last reported, and the only outside signal is `@media (min-device-width:…)`, i.e. the physical screen — which is what the current tiered cap already uses.

| | A — hug, no cap | B — hug up to a cap | C — B + print escape |
|---|---|---|---|
| narrow table | exact fit | exact fit | exact fit |
| wide table | panel scrolls horizontally | scrolls **inside** the box | same as B |
| scrollbars | one (the panel's) | ⚠ two, nested | two on screen |
| jamovi results → PDF | whole table | ⚠ clipped at the cap | whole table |
| white space | never | never | never |

**Decision (maintainer, 2026-08-25): A.** The cap is gone; `max-width` survives only as a 4000 px runaway guard, and `@media print` lifts even that.

⚠ **B's nested scrollbars are not hypothetical.** On a 1920 px screen the current base cap is 1600 px while the results panel is typically 900–1100 px: the user scrolls the *panel* right, and only then meets the *box's* scrollbar. A is one scrollbar and matches what jamovi does with its own wide tables.

Neither option leaves an empty white region: `width:max-content` means a fixed width is never imposed, so the "arbitrary limit that pads every export with white" concern does not arise in any of the three — the cap only ever *removes* width, never adds it.

⚠ Not traced: how results → PDF is produced (the client offers PDF / HTML / LaTeX-zip; the HTML branch ships bytes the client built, `instance.py:_on_save_content`). `@media print { .tx-scrollbox { max-width:none; overflow-x:visible } }` is one cheap line that can only help, and is worth adding whichever option is chosen.

## 5. Vertical space

Nothing further is needed: with every non-Html item at `visible:false`, an inactive item is `position:absolute` (`.jmv-results-item:not([data-active])`) and takes no space at all, and the state round-trip is unaffected — `ResultsElement$asProtoBuf()` serialises `state` in a branch that never reads `visible`. The wasted ~3 cm is the *visible* Image's rendering slot, and the fix above removes the only reason to make one visible.

## 6. What was built

All of it in `R/jmvtab-export.R`, under `# === SECTION: the jamovi results iframe`:

| | |
|---|---|
| `jmv_results_style()` | the one `<style>`: the un-pin, the box, `tx-note`, the print escape |
| `jmv_results_scrollbox()` | wraps a rendered table (moved here from `tab_render_scrollbox()`) |
| `jmv_results_note()` | the one shape a non-table fragment takes |
| `jmv_results_content(...)` | THE boundary: the style once, then the fragments; empties drop out |

```css
.jmv-results-html { width:max-content; }                                    /* the fix */
.tx-scrollbox     { display:block; width:max-content; max-width:4000px; overflow-x:auto; }
.tx-note          { max-width:520px; }
@media print { .tx-scrollbox { max-width:none; overflow-x:visible; } }
```

- all five `html_table$setContent()` calls in the two backends go through `jmv_results_content()`;
- the export status line and both regression placeholders go through `jmv_results_note()` — no backend hand-writes a `<div>` any more;
- `.tabxplor-caption` takes `width:0;min-width:100%`, the idiom `.tx-foot` already used, so a long title cannot size the box either;
- `tab_render_scrollbox()` is deleted from `R/tab-render-html.R` (it was jamovi-only, and its device-width cap tiers went with the cap);
- both `.r.yaml` files are untouched: state carriers stay `visible:false`.

Three gates in `tests/testthat/test-jmvtab-export.R`: the chrome is emitted once and in front; no `.b.R` line hand-writes a `<div>`; every `setContent()` goes through the boundary. Plus the caption rule in `tab_css()`. Full suite **FAIL 0 | PASS 9683**; the only snapshot move is the one caption CSS line.

No `.a.yaml` / `.u.yaml` / `.js` / `.h.R` change, so **no `jmvtools::prepare()`** — `jmvtools::install(home = "flatpak")` ships it.

Live check: a narrow table (the iframe sits at the 620 px floor), a ~1200 px table (iframe ≈ its width, nothing clipped, no scrollbar), a very wide regression table (the results panel scrolls, one scrollbar), the staged-comparison banner alone and the empty-selection hint (must not blow the panel wide), and a successful Excel export (the green "Saved to:" line with a long path).
