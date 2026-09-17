# Release checklist — the release-branch pattern

The permanent branches:

- **`dev`** — the everything-branch: full history + `dev/`, `CLAUDE.md`, `.claude/`, editor config. All development and future bug fixes happen here (the branch used in Positron).
- **`master`** — strictly user-facing: what a visitor or CRAN sees. Never commit to it directly; it only moves by merging release branches.
- **`gh-pages`** — the built pkgdown site, written by CI (`.github/workflows/pkgdown.yaml`) on every push to master. `docs/` is git-ignored; never commit a built site.

## Per release

```bash
git checkout dev && git pull

# 1. Pre-flight on dev. The ORDER matters: each gate reads what the one before it wrote, and the
#    home pages link to release assets that must exist before anything URL-checks them.
#    - Version bumped in DESCRIPTION, NEWS.md section finalized
#    - Home pages regenerated (below), then the jamovi modules published (step 1b), THEN the checks
#    - Full test suite green (the CLAUDE.md § Testing recipe)
#    - Second suite green:  OMP_NUM_THREADS=1 Rscript dev/run_dev_tests.R
#      (the engine-parity sweeps, the source-tree lint and the seam checks the shipped suite
#       keeps only a slice of -- see CLAUDE.md § Testing)
#    - Reverse dependency green:  Rscript dev/revdep_ggfacto.R
#      (CRAN ggfacto and the local checkout, both R CMD check'ed against this tree -- the CRAN one
#       still calls the superseded surface, which is how set_type() was caught at 2.0.0)
#    - urlchecker::url_check(".") clean -- the same tools::check_url_db that R CMD check --as-cran
#      runs, over DESCRIPTION, README.md, NEWS.md, every Rd and the built vignettes. It is what
#      catches a .../releases/latest/download/... link whose release has not been published yet,
#      and a 301, which is also a NOTE.
#    - devtools::check(manual = TRUE, remote = TRUE, incoming = TRUE) green (the ~4 min release
#      gate). NONE of the three arguments is optional, and the bare form is misleading:
#        `manual = TRUE`  -- the default builds no PDF, and "checking PDF version of manual" is the
#                            only step that catches a glyph LaTeX cannot set. Locally:
#                            R CMD Rd2pdf --no-preview --force .  Needs HTML Tidy too
#                            (apt install tidy), or "checking HTML version of manual" only SKIPS.
#        `incoming = TRUE` -- devtools defaults it to `remote`, i.e. FALSE, so the bare call runs
#                            NO URL check and NO CRAN-incoming check. It is the only way to see
#                            locally what CRAN's own incoming machine will say.
#    - Rscript -e 'pkgdown::check_pkgdown()' clean
#    - Generated jamovi files regenerated and committed. R/jmvtab*.h.R are compiler output and
#      they SHIP in the tarball: a stale one makes a declared option read back NULL in the running
#      module, silently, for a whole release (it happened -- `design_effect`). Refresh them either
#      with jmvtools::install(home = 'flatpak') on WSL, or by downloading the `generated-files`
#      artefact from the last jmo workflow run, which needs no local jamovi. Bump
#      jamovi/0000.yaml's version alongside DESCRIPTION's at the same time: CI overrides it per
#      build, a local build does not. The gate, which must print nothing:
#        git status --porcelain -- 'R/*.h.R' inst/i18n jamovi/0000.yaml
#    - Home pages regenerated from their sources, on dev (dev/ is stripped from the release
#      branch, and README.md ships):  OMP_NUM_THREADS=1 Rscript dev/build_readmes.R

# 1b. Publish the jamovi modules BEFORE the checks and before CRAN.
#     The home pages link to .../releases/latest/download/<file>, which 404s until a release
#     carrying the seven .jmo files is PUBLISHED -- and a 404 in README.md is a NOTE from
#     R CMD check --as-cran. `jmo-*` is the module's own tag namespace, so `v*` keeps meaning
#     "a CRAN release" and the module can be rebuilt when a jamovi line moves.
git tag jmo-x.y.z && git push origin jmo-x.y.z
#     The jmo workflow builds seven files into a DRAFT release. Sideload-test at least one Mac
#     file, then publish it -- and only then run the checks above. Publishing does not touch the
#     site: pkgdown deploys from master only.
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
#    - Fill the real CI/rhub run links into cran-comments.md (commit on dev)
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
  `.../releases/latest/download/<name>`, and `latest` is the newest published non-prerelease
  release whatever it holds. Publishing one without them gives students a dead link. A release
  the workflow *creates* is a draft and triggers nothing; one **you** publish rebuilds the site.
  To get files to students before CRAN has accepted, tag `vx.y.z-rc1` and publish it as a
  **pre-release** — which deliberately does not become `latest`, so it needs its own link.
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
- **GitHub Pages is a once-per-repo setup, done AFTER the first merge.** The deploy action
  creates `gh-pages` itself on the first push to `master`; enabling Pages before that branch
  exists is refused. Settings -> Pages -> "Deploy from a branch" -> `gh-pages` / `(root)`, or
  `gh api --method POST repos/<owner>/<repo>/pages -f 'source[branch]=gh-pages' -f
  'source[path]=/'`. Not "GitHub Actions" (it would mean rewriting `pkgdown.yaml` around
  `upload-pages-artifact`/`deploy-pages`, diverging from the r-lib template the workflow came
  from), and never `master`/`docs` -- `docs/` is git-ignored by design. Set the repo `homepage`
  field to the site URL at the same time, or the sidebar shows no link to it.
- **The site must be live before step 5.** Every `bricenocenti.github.io` link in the Rd,
  the README and the vignettes 404 until the pkgdown workflow has deployed from `master` and
  Pages is enabled, and CRAN's incoming check reports them. Merge, confirm the site answers,
  then submit. `urlchecker::url_check(".")` (step 1) is the gate: it runs the same
  `tools::check_url_db` that `R CMD check --as-cran` runs, over the same files, and reports a
  301 as well as a 404 -- a permanent redirect is also a NOTE.
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
