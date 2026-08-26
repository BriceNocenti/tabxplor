# Release checklist — the release-branch pattern

The permanent branches:

- **`dev`** — the everything-branch: full history + `dev/`, `CLAUDE.md`, `.claude/`, editor config. All development and future bug fixes happen here (the branch used in Positron).
- **`master`** — strictly user-facing: what a visitor or CRAN sees. Never commit to it directly; it only moves by merging release branches.
- **`gh-pages`** — the built pkgdown site, written by CI (`.github/workflows/pkgdown.yaml`) on every push to master. `docs/` is git-ignored; never commit a built site.

## Per release

```bash
git checkout dev && git pull

# 1. Pre-flight on dev
#    - Version bumped in DESCRIPTION, NEWS.md section finalized
#    - Full test suite green (the CLAUDE.md § Testing recipe)
#    - Second suite green:  OMP_NUM_THREADS=1 Rscript dev/run_dev_tests.R
#      (the engine-parity sweeps, the source-tree lint and the seam checks the shipped suite
#       keeps only a slice of -- see CLAUDE.md § Testing)
#    - devtools::check() green (the ~3 min release gate)
#    - Rscript -e 'pkgdown::check_pkgdown()' clean

# 2. Branch + strip development-only files
git checkout -b release/x.y.z
git rm -r dev .claude .vscode
git rm CLAUDE.md air.toml
git commit -m "release x.y.z: strip development-only files"

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
```

## Notes

- The strip list (step 2) is the single source of truth for "not on master":
  `dev/`, `.claude/`, `.vscode/`, `CLAUDE.md`, `air.toml`. Everything else stays
  (`jamovi/`, `po/`, `vignettes/articles/`, `_pkgdown.yml`, `.github/`,
  `README.Rmd`, `cran-comments.md`, `.Rbuildignore`). If a new dev-only path
  appears, add it to step 2 here.
- `.Rbuildignore` stays identical on both branches — building the CRAN tarball
  from `dev` must keep working.
- Hotfix on master only if CRAN demands an immediate patch: fix on `dev`,
  then run this same checklist for x.y.z+0.0.1.
