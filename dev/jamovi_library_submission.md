<!--
PURPOSE: Checklist and ready-to-send email for submitting tabxplor to the jamovi module library.
ROLE:    One-off release document for the 2.0.0 jamovi submission. Not part of the package
         (`dev/` is .Rbuildignore'd, so none of this reaches CRAN).
KEY CONSTRAINTS:
  - jamovi builds the module itself for 8 configurations from the GitHub source. The .jmo you
    attach is only a testing convenience -- the source is what is reviewed.
  - Module metadata lives in `jamovi/0000.yaml`. Every build ROUND-TRIPS it: the compiler reads
    it and re-dumps it over itself (`index.js`, `fs.writeFileSync(indexPath, ...)`). Hand edits
    persist, but the YAML is re-wrapped, so expect formatting-only diff noise after a build.
    It is NOT regenerated from DESCRIPTION -- which is exactly how the two drifted apart. Any
    re-sync with DESCRIPTION has to be done by hand.
  - Two build targets, two toolchains. See the build policy table below before rebuilding.
-->

# Submitting tabxplor to the jamovi library

Guidelines followed: <https://dev.jamovi.org/tutorial/tuts0111-distributing-modules>
and <https://dev.jamovi.org/tutorial/tuts0112-additional-notes>.

## Where things stand

Submission is **one email to `contact@jamovi.org`** with a link to the source. jamovi then builds
the module themselves for 8 configurations (solid + current on Windows, solid + current on macOS
Intel and Apple Silicon, current on Linux x64 and ARM) and reviews it. Expect **1-2 weeks**;
library updates are pushed weekly, on Mondays.

### Release channels, read from jamovi.org/versions.json on 2026-08-29

| Channel | macOS    | Windows  | Linux    |
|---------|----------|----------|----------|
| current | 28.2.0.0 | 28.2.0.0 | 28.2.0.0 |
| solid   | 2.7.38.0 | 2.7.38.0 | *(none)* |
| legacy  | 2.6.45.0 | 2.6.44.0 | *(none)* |

Three things follow from that table:

- jamovi renumbered after 2.7.38 (2026-07-21). The new series is **28.x** — not 2.8.x.
- **Linux has no solid channel.** Linux users only ever get *current*, so 28.x is the only thing
  that matters there.
- The series pins the R version, and `library.jamovi.org` is indexed by it: jamovi 2.7 → R 4.5.0,
  jamovi 28.x → R 4.6.0. Modules build against a **frozen CRAN snapshot** per series, so the series
  — not just the app version — decides what a test actually covers.

### Build and test policy

| Machine | jamovi                 | R     | jmvtools        | Build dir                 |
|---------|------------------------|-------|-----------------|---------------------------|
| Windows | solid, 2.7.37 → 2.7.38 | 4.5.0 | 2.7.26 (pinned) | `build/R4.5.0-x64-win64/` |
| WSL2    | current, 28.2          | 4.6.0 | 28.2 (pinned)   | `build/R4.6.0-x64-linux/` |

Between them the two machines cover both lines jamovi builds for. Pin jmvtools to the app version
on each: `repo.jamovi.org` serves 2.7.26, 28.0, 28.1, 28.2 and 28.3, so a bare
`install.packages("jmvtools", repos = "https://repo.jamovi.org")` silently takes 28.3.

The two toolchains do **not** conflict. The bundled `jamovi-compiler` in 2.7.26 and in 28.2 both
hard-pin `jms: '1.0'` and both accept `jas` in `(1.1, 1.2]`, so `jamovi/0000.yaml`, the `.a.yaml`
and the `.u.yaml` files stay valid for both, and whichever machine builds last does not break the
other. The old note in `dev/build_jmo_windows.R` claiming 28.x "emits a `jms` version jamovi
refuses" is not true of 28.2 — keep the Windows pin anyway, but do not propagate that reason.

### WSL build gotchas

- **The freedesktop Sdk must match jamovi's runtime.** `jmvtools::install(home = "flatpak")` shells
  out to `flatpak run --devel org.jamovi.jamovi -R --version`, and `--devel` needs the *Sdk*, not
  just the Platform runtime. jamovi 28.2 moved from the 24.08 runtime to **25.08**, and a
  `flatpak update` pulls only the Platform, so every build died with
  `error: runtime/org.freedesktop.Sdk/x86_64/25.08 not installed`. Fix:
  `flatpak install -y flathub org.freedesktop.Sdk//25.08`. Re-check after any jamovi update with
  `flatpak info org.jamovi.jamovi | grep Runtime`.
- **`ELECTRON_RUN_AS_NODE` must be unset on the host**, not merely inside the flatpak — see the
  header of `~/.local/bin/jamovi`.
- **Rolling back to 2.7.36 means re-downloading it.** The update pruned the old deploy, but flathub
  still carries the commit, so
  `flatpak update --commit=56eb8de3d468e093ac25cf0bb6236c51e0828fb1b5e8e5bce7b3df110cf49240 org.jamovi.jamovi`
  works.

One real 28.x change: the compiler dropped the `'catalog'` → `'c'` shorthand and no longer
lowercases language codes, so `jmvtools::i18nCreate('catalog')` would now create a *language* named
"catalog". Use `jmvtools::i18nUpdate()` and explicit codes such as `'fr'`.

## Already compliant, nothing to do

- `License: GPL (>= 3)` — OSI approved, and not AGPL3, which jamovi refuses.
- Module name `tabxplor`: distinctive, no dot, does not embed "jamovi".
- `jamovi/0000.yaml` `version: 2.0.0` matches `DESCRIPTION`; semantic versioning throughout.
- `URL` and `BugReports` set; README mentions the jamovi module.
- No `Remotes:`. Every dependency floor is conservative (`dplyr >= 1.0.3`, `ggplot2 >= 3.5.0`,
  `marginaleffects >= 0.20.0`), so any 2025-or-later CRAN snapshot satisfies them.
- No build artifacts committed: `*.jmo`, `build/` and `build*/` are in `.gitignore`, and
  `git ls-files` confirms nothing built is tracked.
- `.Rbuildignore` carries `^jamovi/` and `[.]jmo$`, keeping the CRAN tarball and the jamovi source
  tree cleanly separate — correct on both sides.
- Unit tests exist for both analyses: `tests/testthat/test-jmvtab.R`, `test-jmvtabreg.R`.
- English is the base language; French ships as `jamovi/i18n/fr.po` (288 msgids), compiled to
  `inst/i18n/fr.json` by `jmvtools::install()`.
- Size is a non-issue: the library already hosts a 99 MB module
  (`ClinicoPathDescriptives-0.0.51.jmo`).
- No module icon is needed — the module-definition API has no icon key.

## To do

### Must, before sending

- [x] Replace jamovi 2.7.36 with 28.2 on WSL2 (`flatpak update org.jamovi.jamovi`), and jmvtools
      2.7.26 with 28.2.
- [x] Align the drifted metadata in `jamovi/0000.yaml`: title was missing "User-Friendly",
      "observed/crude" → "crude/observed", a double space in "colors  to 'Excel'", stale `date`.
- [x] `jamovi/jmvtabreg.r.yaml`: `title: Regression models` → `Regressions`, matching every other
      file.
- [x] `minApp: 1.0.8` → `2.4.0`. Both `.a.yaml` declare `weightsSupport: 'full'`, and weights only
      arrived in jamovi 2.4.0, so 1.0.8 was an unmet promise. Both build targets clear the new
      floor.
- [x] Drop the stray trailing comma after `MASS (>= 7.3.0),` in `DESCRIPTION`. Harmless — R parses
      18 entries and discards the empty one — but it should not go out in a submission.
- [x] Clean rebuild of the `.jmo` from a wiped build library — see results below. Not for the
      megabytes: it **proved `DESCRIPTION` alone is enough to build the module**, which is exactly
      what jamovi's build farm does.
- [x] Automated checks on 28.2: the module builds, installs into jamovi 28.2
      (`~/.jamovi/modules/tabxplor/`), and `devtools::test(filter = "jmv")` passes **115 / 115**
      under R 4.6.1.
- [x] **Manual GUI session on jamovi 28.2** — UI confirmed correct on 28.x. Two 28.x rendering
      changes were found and fixed: dropdown *choices* no longer strip `<i>...</i>` (46 titles
      stripped across both `.a.yaml`, French kept and stripped to match), and `<b>` no longer
      renders in variable-selector labels (left in place — harmless, and the tags were already
      present at every layer, so it is a jamovi rendering change, not missing markup).
- [ ] **Publish the changes to `master`** — this is the actual gate. jamovi builds from the repo's
      default branch, so nothing is submitted until the fixes land there. `dev` and `master` have
      diverged: `master` still has `minApp: 1.0.8`, `title: Regression models` and the un-stripped
      `<i>` dropdowns. Follow the established flow — branch `release/*` off `dev`, strip the
      development-only files (`CLAUDE.md`, `dev/`), PR to `master` — then push.
- [ ] Host `tabxplor_2.0.0.jmo` and paste the URL over `<LINK>` in the email.

> `jamovi/0000.yaml` is round-tripped by every build, not regenerated from `DESCRIPTION`. That is
> why the title and description drifted and stayed drifted. After any future `DESCRIPTION` edit,
> re-sync `0000.yaml` by hand and check `git diff` — a build will also re-wrap the YAML, so expect
> some formatting-only noise alongside the real change.

### Rebuild results, 2026-08-29

Built against jamovi 28.2 / R 4.6.0 with jmvtools 28.2:

|                   | Before (R 4.5.0) | After (R 4.6.0) |
|-------------------|------------------|-----------------|
| `.jmo` size       | 61.9 MB          | **23.3 MB**     |
| Files in archive  | 2716             | 498             |
| Uncompressed      | 132 MB           | 40.8 MB         |
| Vendored packages | 54               | **16**          |

The 16 that remain are all genuinely needed: `VGAM`, `svyVGAM`, `brant` (ordinal regressions),
`survey`, `mitools` (weighted data), `marginaleffects`, `insight`, `openxlsx2` (Excel export),
`mirai`, `nanonext`, `parallelly`, `RhpcBLASctl` (parallelism), plus `DBI`, `fansi`, `clipr` and
`tabxplor` itself. `dplyr`, `ggplot2` and the rest of the tidyverse are no longer vendored at all —
jamovi 28.x already bundles them, so the module stops shipping duplicates.

The old artifacts are kept, not deleted: `tabxplor_2.0.0_R4.5.0-x64-linux.jmo` and `build_old/`.

**The file to send is `tabxplor_2.0.0.jmo`** at the repo root — 23.3 MB, Linux x64, R 4.6.0.

### Should, soon but not blocking

- [ ] `dev/build_jmo_windows.R` is present on `dev` (it is stripped only on the release line by
      `71d6f1b`, so it is absent from `master`). Correct its `jms` comment per the section above
      before the next Windows build — the rest of it (jmvtools 2.7.26 pin, `ELECTRON_RUN_AS_NODE`
      unset, Windows DLL-lock fix) is still accurate.
- [ ] Rebuild the Windows `.jmo` once, so the metadata fixes above are verified on that target too.
- [ ] The guard tests `test-jamovi-vocabulary.R` and `test-jamovi-i18n.R` were dropped in `c3b5ef8`
      ("Phase 23e — Tests simplification"), so the generated blocks in `jamovi/js/*.js` and the
      `_()` coverage are unguarded. `Rscript dev/generate_jamovi_js.R check` still works and
      currently reports "generated blocks are up to date" — worth wiring back into the suite.

### Deliberately not doing now

- **Module datasets.** The checklist suggests shipping example data as `.omv` or `.csv` in `data/`,
  declared under a `datasets:` key in `0000.yaml`. The four `data/*.rda` are R datasets, not jamovi
  ones, so this is real work. Worth doing for 2.1.0; it would only delay this submission.
- **Bumping the analyses' `version: '1.0.0'`.** It has not moved across ~96 commits, so it is
  arguably untrue — but this is the *first published* release of both analyses, and the analysis
  version is what jamovi uses to migrate saved `.omv` files. Starting at 1.0.0 in the library is
  cleaner than shipping 1.1.0 with no 1.0.0 ever released.
- **Dropping the "All functions render data frames…" sentence** from the jamovi description was
  right and stays. That sentence is meaningful to R users and meaningless in the jamovi library
  card, so `0000.yaml` and `DESCRIPTION` are aligned in wording but not identical in scope.

### After sending

- [ ] Expect a reply in 1-2 weeks, by email or as a GitHub issue.
- [ ] Renaming a module after submission is costly — `tabxplor` is final.
- [ ] For future updates, bump the version in **both** `DESCRIPTION` and `jamovi/0000.yaml`.

## The email

To: `contact@jamovi.org`
Subject: Module submission — tabxplor 2.0.0 (crosstables and regression tables)

Replace `<LINK>` with wherever you host the `.jmo`. **The "tested on jamovi 28.2 (Linux)" line is
only accurate once the manual GUI session above is done** — the build and the unit tests pass on
28.2, but the UI has not been driven by hand there yet. Either run that session first, or soften the
line to say 2.7.36 / 2.7.37 were the manually tested versions.

```text
Hello,

I would like to submit tabxplor to the jamovi library.

  Module   : tabxplor, version 2.0.0
  Source   : https://github.com/BriceNocenti/tabxplor (branch master)
  Licence  : GPL (>= 3)
  Analyses : "Crosstables" (jmvtab) and "Regressions" (jmvtabreg),
             both under the "tabxplor" menu group
  Docs     : https://bricenocenti.github.io/tabxplor/

tabxplor builds cross-tables for data exploration, with colour helpers that
highlight deviations (differences from totals, comparisons between rows or
columns, contributions to variance, odds ratios) and significance (confidence
intervals, stars). The same grammar applies to regression models, so a model's
adjusted effects can be compared systematically with their crude counterparts.
Survey weights are fully supported, and tables can be exported to Excel, HTML
and markdown. The R package is on CRAN (submitted, currently pending review).

Both analyses have been tested on jamovi 28.2 (Linux) and 2.7.37 (Windows),
including datasets with missing values, empty selections, accented and special
characters in variable names, and weighted data. Both implement input checks
and fail gracefully rather than crashing. The module ships a French UI
translation alongside English.

A Linux x64 build (R 4.6.0) is available here if it helps for testing:
<LINK>

Happy to make any change you need.

Best regards,
Brice Nocenti
```
