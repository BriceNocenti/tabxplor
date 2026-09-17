# Release checklist — the release-branch pattern

The permanent branches:

- **`dev`** — the everything-branch: full history + `dev/`, `CLAUDE.md`, `.claude/`, editor config. All development and future bug fixes happen here (the branch used in Positron).
- **`master`** — strictly user-facing: what a visitor or CRAN sees. Never commit to it directly; it only moves by merging release branches.
- **`gh-pages`** — the built pkgdown site, written by CI (`.github/workflows/pkgdown.yaml`) on every push to master. `docs/` is git-ignored; never commit a built site.

## Per release

```bash
git checkout dev && git pull

# 1. Pre-flight on dev, IN THIS ORDER. Each gate reads what the one before it wrote, and the home
#    pages link to release assets that must exist before anything URL-checks them.
#
#    a) Version bumped in DESCRIPTION *and* in jamovi/0000.yaml (hand-duplicated: CI overrides it
#       per build, a local build does not), NEWS.md section finalized.
#    b) Generated jamovi files regenerated and committed. R/jmvtab*.h.R are compiler output and
#       they SHIP in the tarball: a stale one makes a declared option read back NULL in the running
#       module, silently, for a whole release (it happened -- `design_effect`). Refresh them either
#       with jmvtools::install(home = 'flatpak') on WSL, or by downloading the `generated-files`
#       artefact from the last jmo workflow run, which needs no local jamovi. The gate, which must
#       print nothing:
#         git status --porcelain -- 'R/*.h.R' inst/i18n jamovi/0000.yaml
#    c) Home pages regenerated from their sources (dev/ is stripped from the release branch, and
#       README.md ships):  OMP_NUM_THREADS=1 Rscript dev/build_readmes.R
#    d) >>> step 1b: publish the jamovi modules. Everything below reads the links they provide. <<<
#    e) Full test suite green (the CLAUDE.md § Testing recipe).
#    f) Second suite green:  OMP_NUM_THREADS=1 Rscript dev/run_dev_tests.R
#       (the engine-parity sweeps, the source-tree lint and the seam checks the shipped suite
#        keeps only a slice of -- see CLAUDE.md § Testing)
#    g) Reverse dependency green:  Rscript dev/revdep_ggfacto.R
#       (CRAN ggfacto and the local checkout, both R CMD check'ed against this tree -- the CRAN one
#        still calls the superseded surface, which is how set_type() was caught at 2.0.0. Its own
#        "missing Depends: R (>= 4.1.0)" NOTE is ggfacto's, not ours.)
#    h) urlchecker::url_check(".") clean -- the same tools::check_url_db that R CMD check --as-cran
#       runs, over DESCRIPTION, README.md, NEWS.md, every Rd and the built vignettes. It is what
#       catches a .../releases/latest/download/... link whose release is not published yet, and a
#       301, which is also a NOTE.
#    i) devtools::check(manual = TRUE, remote = TRUE, incoming = TRUE) green (~4 min). NONE of the
#       three arguments is optional, and the bare form is misleading:
#         `manual = TRUE`   -- the default builds no PDF, and "checking PDF version of manual" is
#                              the only step that catches a glyph LaTeX cannot set. Locally:
#                              R CMD Rd2pdf --no-preview --force .  Needs HTML Tidy too
#                              (apt install tidy), or "checking HTML version of manual" only SKIPS.
#         `incoming = TRUE` -- devtools defaults it to `remote`, i.e. FALSE, so the bare call runs
#                              NO URL check and NO CRAN-incoming check. It is the only way to see
#                              locally what CRAN's own incoming machine will say.
#    j) Rscript -e 'pkgdown::check_pkgdown()' clean.
#
#    The push of (a)-(c) also starts R-CMD-check on 5 platforms; that run is the GitHub Actions
#    link cran-comments.md wants (step 5).

# 1b. Publish the jamovi modules BEFORE the checks and before CRAN.
#     The home pages link to .../releases/latest/download/<file>, which 404s until a release
#     carrying the seven .jmo files is PUBLISHED -- and a 404 in README.md is a NOTE from
#     R CMD check --as-cran. `jmo-*` is the module's own tag namespace, so `v*` keeps meaning
#     "a CRAN release" and the module can be rebuilt when a jamovi line moves.
git tag jmo-x.y.z && git push origin jmo-x.y.z
#     The jmo workflow builds the seven files and opens a DRAFT release carrying them. Publishing
#     does not touch the site: pkgdown deploys from master only.
#     ⚠ Rehearse on a scratch branch FIRST whenever the workflow, a jamovi line or a dependency has
#     moved -- `git push origin dev:ci/jmo` builds all seven without creating a release. That is
#     what `ci/**` is for, and at 2.0.1 it took eight rounds to get green.
#     ⚠ Sideload-test at least one Mac file if a Mac is at hand. At 2.0.1 none was: CI proves the
#     files load and compute in jamovi's own R, not that jamovi's installer accepts them, so the
#     students were the test. That is an acceptable risk precisely because `jmo-*` is independent
#     of CRAN -- a bad build is re-cut as jmo-x.y.z+1 the same day.
gh run watch && gh release edit jmo-x.y.z --draft=false

# 2. Branch + strip development-only files
git checkout -b release/x.y.z
git rm -r dev .claude .vscode
git rm CLAUDE.md air.toml
git commit -m "release x.y.z: strip development-only files"

# 2b. Prove the release tree IS dev minus the strip list. BOTH must print nothing.
#     Together they are what makes "dev-green means release-green" a fact, not a hope.
#     NOTE `refs/heads/dev`, not `dev`: here the name is a revision AND the directory
#     just stripped, and git refuses the ambiguity.
git diff --name-only refs/heads/dev HEAD -- \
  | grep -vE '^(dev/|\.claude/|\.vscode/|CLAUDE\.md$|air\.toml$)'
git ls-files -- dev .claude .vscode CLAUDE.md air.toml

# 3. PR
git push -u origin release/x.y.z
gh pr create --base master --title "tabxplor x.y.z" --body "<NEWS summary>"

# 4. Wait for CI green (R-CMD-check + pkgdown build), then merge.
#    ALWAYS a merge commit, NEVER squash/rebase: squash breaks the merge-base,
#    so the next release merge would re-conflict on every dev-only file.
#    With merge commits, the next release's removals resolve as clean
#    delete/delete against master.
gh pr merge --merge
git push origin --delete release/x.y.z
git branch -D release/x.y.z   # denied in Claude sessions: run in your own terminal

# 5. CRAN
#    - Fill the three <FILL> links in cran-comments.md (commit on dev): the R-CMD-check run from
#      step 1, the rhub run, and the win-builder result mailed to the maintainer address.
#      ⚠ rhub and win-builder must run AFTER step 1b: they URL-check README.md too.
#    - devtools::submit_cran() (regenerates CRAN-SUBMISSION) or the web form

# 6. After CRAN acceptance
git tag vx.y.z <merge-commit-sha>
git push origin vx.y.z

# 7. The jamovi modules, again
#    The v tag starts .github/workflows/jmo.yaml a second time and opens a DRAFT release carrying
#    the same seven files. Publish it: `latest` is the newest published release whatever it holds,
#    so a published release WITHOUT them turns every download link in the README into a dead one.
gh run watch && gh release edit vx.y.z --draft=false
```

## Notes

- **Every published release must carry the seven `.jmo` files**, because the course links are
  `.../releases/latest/download/<name>` and `latest` is the newest published non-prerelease release
  *whatever it holds*. Publishing one without them gives students a dead link — which is why the
  workflow refuses an incomplete set and why both `jmo-*` and `v*` tags build the same seven.
  ⚠ Publishing a release does **not** rebuild the site: `pkgdown.yaml` deliberately has no
  `release:` trigger, because a release is a tag and the site is `master`'s — publishing a `jmo-*`
  tagged on `dev` would otherwise replace the live site with a development build.
- **A module release is not a package release.** `jmo-*` is the module's own namespace, so the
  seven files can be re-cut the day a jamovi line moves, a build proves bad on a student's machine,
  or the panel needs a fix — with no CRAN release and no version bump. `v*` stays "a CRAN release".
- The strip list (step 2) is the single source of truth for "not on master":
  `dev/`, `.claude/`, `.vscode/`, `CLAUDE.md`, `air.toml`. Everything else stays
  (`jamovi/`, `po/`, `vignettes/articles/`, `_pkgdown.yml`, `.github/`,
  `README.Rmd`, `cran-comments.md`, `.Rbuildignore`). If a new dev-only path
  appears, add it to step 2 here.
- **Re-cutting a stale release branch** (its tree is months behind, or predates a directory
  that now ships): `gh pr comment` the reason and `gh pr close` the PR, `git push origin
  --delete release/x.y.z`, then step 2 as written. Deleting the remote branch of an open PR
  auto-closes it, so close it first with the reason recorded. The maintainer runs `git branch
  -D release/x.y.z` in their own terminal -- twice, once to free the name and once after the
  merge -- and it fails with `cannot delete branch used by worktree` unless the checkout has
  been moved off it (`git checkout dev`).
- **Two things must be live before step 5**, and one gate covers both: the pkgdown site (every
  `bricenocenti.github.io` link in the Rd, the README and the vignettes 404 until it has deployed
  from `master`) and the `jmo-*` release (step 1b). `urlchecker::url_check(".")` is that gate — the
  same `tools::check_url_db` CRAN runs, over the same files, reporting a 301 as well as a 404.
- **rhub: the compiler containers say nothing here.** tabxplor has no `src/`, so `clang*`,
  `gcc*`, `c23`, `lto`, `*-asan`, `valgrind` and `rchk` only exercise a toolchain the package
  never uses -- and a stale image there fails on a *dependency* (`clang19`/`clang20` carry an
  R-devel from 2026-03 that vctrs 0.7.2 will not load on). Use the platforms that vary the
  RUNTIME instead: `nosuggests` (the 25 Suggests and their `tx_need_pkg()` gates), `nold`,
  `atlas`, `mkl`, `donttest`, `ubuntu-next`, `ubuntu-release`.
- **`nosuggests` is the one worth rehearsing here first, and it is a real gate.** Its whole
  mechanism is `_R_CHECK_DEPENDS_ONLY_=true`, which `tools:::.check_packages` alone reads -- so it
  bites at CHECK time and reproduces locally, no rhub involved:
  `withr::with_envvar(c("_R_CHECK_DEPENDS_ONLY_" = "true"), devtools::check(manual = TRUE))`.
  ⚠ What it hides is `Depends + Imports + VignetteBuilder`, plus testthat for the tests step
  (`tools:::setRlibs`) -- so anything else a TEST or a VIGNETTE reaches for must be guarded, the
  same way every Rd example already is. Run it before a release; it takes 3 min and it is the
  check most likely to find something.
  If the rhub job instead sits silent for hours and dies at GitHub's 6 h limit, that is a stalled
  container, not a result: `R CMD build` never reads that variable, so the build it hangs in is
  the one every other platform finishes in ~90 s. Cancel it and re-run the platform alone.
- `.Rbuildignore` stays identical on both branches — building the CRAN tarball
  from `dev` must keep working.
- Hotfix on master only if CRAN demands an immediate patch: fix on `dev`,
  then run this same checklist for x.y.z+0.0.1.
