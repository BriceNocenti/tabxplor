# PURPOSE: Fully-automated Windows-side build + verify of the tabxplor jamovi `.jmo`.
# ROLE:    Release build path for the WINDOWS `.jmo` (roadmap Phase 15a). Clones the current
#          repo into a THROWAWAY temp folder (never the D:/Statistiques/github "build-only"
#          checkout), installs the toolchain + deps, and runs jmvtools::install() to produce
#          and install the module into a Windows jamovi (default 2.7.37.0), then verifies it.
# KEY CONSTRAINTS:
#   - WINDOWS ONLY. Aborts on any non-Windows OS (a Linux `.jmo` is not a Windows `.jmo`).
#   - `jmvtools` MUST be 2.7.26. The repo.jamovi.org index also serves 28.x, which R would
#     pick and which emits a `jms` version jamovi refuses -> we install the pinned tarball.
#   - `ELECTRON_RUN_AS_NODE` must be UNSET before install() (Positron/Claude Code export it).
#   - `jmvtools::check()` proves nothing (never reaches Electron); trust install() + the
#     landed-module verify, not the check.
#   - DLL LOCK: install() leaves jamovi engine processes running that memory-map the module's DLLs;
#     the next build then can't overwrite them ("Permission denied ...\marginaleffects.dll"). We kill
#     jamovi processes (by exe path) + delete the stale module dir BEFORE install and kill again AFTER
#     (CFG$stop_jamovi / clean_module). A fresh R session does NOT clear these OS processes.
# See: dev/jamovi_module.md section 2 ; CLAUDE.md "Jamovi module development".
#
# USAGE (on Windows 11, R 4.6.1):
#   Rscript dev/build_jmo_windows.R
#   Rscript dev/build_jmo_windows.R master                 # override the branch (positional)
#   set TABXPLOR_BRANCH=v2.0.0 && Rscript dev/build_jmo_windows.R
#   set JAMOVI_HOME=C:/Program Files/jamovi 2.7.37.0 && Rscript dev/build_jmo_windows.R
#
# NOTE: This script is authored in WSL but only runs on Windows. It is dev/-only (.Rbuildignore'd)
#       and never ships to CRAN. Do not source it from tests/.

# === CONFIG: everything you might want to change lives here =========================

CFG <- list(
  # --- source to build ---------------------------------------------------------------
  repo_url = "https://github.com/BriceNocenti/tabxplor.git",
  # Branch: CLI arg 1 > env TABXPLOR_BRANCH > this default.
  branch   = "master", # "dev"

  # --- target jamovi -----------------------------------------------------------------
  # Windows jamovi install folder. Auto-detected under C:/Program Files/jamovi*, preferring
  # this exact version, else the newest match; falls back to this literal path. Override with
  # the JAMOVI_HOME env var or options(jamovi_home=) or by editing this line.
  jamovi_home_default = "C:/Program Files/jamovi 2.7.37.0",
  jamovi_glob         = "C:/Program Files/jamovi*",
  jamovi_prefer       = "2.7.37.0",

  # --- toolchain pin (do not "fix" the version) --------------------------------------
  jmvtools_version = "2.7.26",
  jmvtools_tarball = "https://repo.jamovi.org/src/contrib/jmvtools_2.7.26.tar.gz",
  jamovi_repo      = "https://repo.jamovi.org",
  cran_repo        = "https://cloud.r-project.org",
  force_jmvtools   = FALSE,   # TRUE = reinstall node + pinned jmvtools even if already correct

  # --- dependencies ------------------------------------------------------------------
  # `dependencies = NA` = Depends/Imports/LinkingTo (the minimum load_all() needs).
  # jmvcore is a Suggests but the jamovi backend (R/jmvtab.b.R) needs it -> installed explicitly.
  dep_which  = NA,
  extra_deps = c("jmvcore"),
  skip_deps  = FALSE,        # TRUE = assume deps are already present, skip install_deps

  # --- housekeeping ------------------------------------------------------------------
  keep_clone   = TRUE,       # keep the temp clone so you can inspect / re-run; FALSE deletes it
  use_pak      = TRUE,       # STAGE 3: install R deps with pak (robust vs base install.packages);
                             #   auto-falls back to devtools::install_deps() when pak is absent.
  stop_jamovi  = TRUE,       # STAGE 4 + cleanup: kill lingering jamovi/engine processes that lock
                             #   module DLLs -- the recurring "Permission denied ... .dll" cause.
  clean_module = TRUE        # STAGE 4: delete the stale installed tabxplor module dir before install
                             #   so there is no locked DLL to overwrite (also clears a half-install).
)

# === Small logging + failure helpers ================================================

banner <- function(msg) {
  cat("\n", strrep("=", 78), "\n== ", msg, "\n", strrep("=", 78), "\n", sep = "")
}
step <- function(msg) cat("--  ", msg, "\n", sep = "")
die  <- function(...) stop(paste0(...), call. = FALSE)

need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    die("Required package '", pkg, "' is not installed in this R.\n",
        "     Install it first, e.g.:  install.packages(\"", pkg, "\")")
  }
}

# === Jamovi process + stale-module helpers (THE DLL-lock fix) =======================
# WHY: jmvtools::install() spawns a headless jamovi (server + engine workers). On Windows those
# engine processes LOAD the module's compiled DLLs (marginaleffects.dll, VGAM.dll, ...) and do NOT
# reliably exit. The next build's install-from-file then cannot OVERWRITE a memory-mapped DLL ->
# "PermissionError: [Errno 13] Permission denied: ...\modules\tabxplor\R\marginaleffects\libs\x64\
# marginaleffects.dll". A fresh R session does NOT help -- these are separate OS processes. So we
# kill them BEFORE and AFTER install, and delete the stale installed module dir first (nothing to
# overwrite = no lock). Killing by EXECUTABLE PATH under the jamovi / %APPDATA%\jamovi roots is
# name-agnostic (catches jamovi.exe, the engine, its bundled R) and never hits this R -- its exe is
# not under those roots, and we exclude our own PID.

jamovi_module_roots <- function(jamovi_home) {
  home_user <- Sys.getenv("USERPROFILE", unset = path.expand("~"))
  unique(c(
    file.path(home_user, "AppData", "Local",   "jamovi", "modules"),
    file.path(home_user, "AppData", "Roaming", "jamovi", "modules"),
    file.path(home_user, ".jamovi", "modules"),
    file.path(Sys.getenv("LOCALAPPDATA", ""), "jamovi", "modules"),
    file.path(Sys.getenv("APPDATA", ""),      "jamovi", "modules"),
    file.path(dirname(jamovi_home), "modules"),
    file.path(jamovi_home, "modules")
  ))
}

# Kill every process whose executable lives under the jamovi install or %APPDATA%\jamovi (plus the
# jamovi-server python), excluding THIS R process. Path-scoped so it can't touch Positron/Rterm.
stop_jamovi_processes <- function(jamovi_home) {
  if (.Platform$OS.type != "windows") return(invisible())
  ps1 <- tempfile(fileext = ".ps1")
  writeLines(c(
    "param([int]$self, [string]$jhome)",
    "$prefixes = @($jhome, \"$env:APPDATA\\jamovi\", \"$env:LOCALAPPDATA\\jamovi\") |",
    "  Where-Object { $_ -and $_.Length -gt 0 }",
    "$killed = @()",
    "Get-CimInstance Win32_Process | ForEach-Object {",
    "  $p = $_",
    "  if ($p.ProcessId -eq $self -or -not $p.ExecutablePath) { return }",
    "  foreach ($pre in $prefixes) {",
    "    if ($p.ExecutablePath.ToLower().StartsWith($pre.ToLower())) {",
    "      try { Stop-Process -Id $p.ProcessId -Force -ErrorAction Stop",
    "            $killed += \"$($p.ProcessId) $($p.Name)\" } catch {}",
    "      break",
    "    }",
    "  }",
    "}",
    "Get-CimInstance Win32_Process | Where-Object {",
    "  $_.ProcessId -ne $self -and $_.Name -eq 'python.exe' -and $_.CommandLine -like '*jamovi*'",
    "} | ForEach-Object {",
    "  try { Stop-Process -Id $_.ProcessId -Force -ErrorAction Stop",
    "        $killed += \"$($_.ProcessId) python(jamovi)\" } catch {} }",
    "if ($killed.Count) { 'killed: ' + ($killed -join '; ') } else { 'none running' }"
  ), ps1)
  out <- tryCatch(
    system2("powershell",
            c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(ps1),
              "-self", Sys.getpid(), "-jhome", shQuote(jamovi_home)),
            stdout = TRUE, stderr = TRUE),
    error = function(e) paste("powershell unavailable:", conditionMessage(e)))
  unlink(ps1)
  step(paste0("stop jamovi processes: ", paste(out, collapse = " ")))
  Sys.sleep(2)   # give Windows a moment to release the DLL handles
  invisible()
}

# Delete the stale installed <root>/tabxplor module dir(s), so install-from-file writes into a clean
# folder (no locked DLL to overwrite, and a prior half-install can't poison jamovi's module scan).
remove_installed_module <- function(jamovi_home, name = "tabxplor") {
  for (root in jamovi_module_roots(jamovi_home)) {
    d <- file.path(root, name)
    if (dir.exists(d)) {
      suppressWarnings(unlink(d, recursive = TRUE, force = TRUE))
      step(paste0(if (!dir.exists(d)) "removed stale module: "
                  else "could NOT fully remove (still locked -- kill processes first?): ", d))
    }
  }
  invisible()
}

# === STAGE 0: environment sanity ====================================================

banner("tabxplor Windows .jmo builder")

if (.Platform$OS.type != "windows") {
  die("This script builds a WINDOWS .jmo and must run on Windows.\n",
      "     Detected OS: ", Sys.info()[["sysname"]], ".\n",
      "     On Linux/WSL use instead:  jmvtools::install(home = 'flatpak')")
}

step(paste0("R ", getRversion(), " on ", Sys.info()[["sysname"]]))

# devtools drives load_all()/install_deps; pkgbuild reports build-tool availability.
need_pkg("devtools")
need_pkg("pkgbuild")

# git must be reachable (Git for Windows on PATH) for the clone.
git_ok <- tryCatch(system2("git", "--version", stdout = TRUE, stderr = TRUE),
                   error = function(e) NA_character_)
if (length(git_ok) == 0 || is.na(git_ok[1])) {
  die("`git` was not found on PATH. Install Git for Windows and reopen the shell.")
}
step(git_ok[1])

# Resolve the branch: CLI arg 1 > env > default.
cli_args <- commandArgs(trailingOnly = TRUE)
branch <- if (length(cli_args) >= 1 && nzchar(cli_args[1])) {
  cli_args[1]
} else if (nzchar(Sys.getenv("TABXPLOR_BRANCH"))) {
  Sys.getenv("TABXPLOR_BRANCH")
} else {
  CFG$branch
}
step(paste0("Branch to build: ", branch))

# Resolve the jamovi home: env JAMOVI_HOME > options(jamovi_home) > auto-detect > default.
resolve_jamovi_home <- function() {
  env  <- Sys.getenv("JAMOVI_HOME")
  if (nzchar(env)) return(env)
  opt <- getOption("jamovi_home")
  if (!is.null(opt) && nzchar(opt)) return(opt)
  hits <- Sys.glob(CFG$jamovi_glob)
  hits <- hits[dir.exists(hits)]
  if (length(hits)) {
    pref <- hits[grepl(CFG$jamovi_prefer, hits, fixed = TRUE)]
    if (length(pref)) return(pref[1])
    return(sort(hits, decreasing = TRUE)[1])   # newest-looking match
  }
  CFG$jamovi_home_default
}
jamovi_home <- resolve_jamovi_home()
step(paste0("jamovi home: ", jamovi_home))
if (!dir.exists(jamovi_home)) {
  cat("!!  WARNING: that jamovi folder does not exist. jmvtools::install() will fail.\n",
      "!!  Set JAMOVI_HOME to your real install path, e.g.:\n",
      "!!    set JAMOVI_HOME=C:/Program Files/jamovi 2.7.37.0\n", sep = "")
}

# Windows CRAN deps are normally binaries; a source dep would need Rtools. Warn, don't abort.
if (!pkgbuild::has_build_tools(debug = FALSE)) {
  cat("!!  Rtools not detected. Most deps install as Windows binaries, but if any dependency\n",
      "!!  must build from source it will fail. Install Rtools if that happens.\n", sep = "")
}

# === STAGE 1: clone into a throwaway temp folder ====================================

banner("STAGE 1/5  Clone")

clone_dir <- tempfile("tabxplor_jmo_")
dir.create(clone_dir, recursive = TRUE, showWarnings = FALSE)
step(paste0("Cloning into: ", clone_dir))

rc <- system2("git",
              c("clone", "--depth", "1", "--branch", shQuote(branch),
                shQuote(CFG$repo_url), shQuote(clone_dir)))
if (rc != 0 || !file.exists(file.path(clone_dir, "DESCRIPTION"))) {
  die("git clone failed (rc=", rc, "). Check the branch name and network access.")
}
step("Clone OK (DESCRIPTION present).")

# === STAGE 2: toolchain (node + pinned jmvtools 2.7.26) =============================

banner("STAGE 2/5  Toolchain: node + jmvtools (pinned 2.7.26)")

jmvtools_ok <- requireNamespace("jmvtools", quietly = TRUE) &&
  as.character(utils::packageVersion("jmvtools")) == CFG$jmvtools_version

if (jmvtools_ok && !CFG$force_jmvtools) {
  step(paste0("jmvtools ", CFG$jmvtools_version, " already installed; skipping (set ",
              "force_jmvtools=TRUE to reinstall)."))
} else {
  # node FIRST: the pinned jmvtools tarball is installed with repos=NULL (no dep resolution),
  # so `node` must already be present. node comes from the jamovi repo, not CRAN.
  step("Installing 'node' from repo.jamovi.org ...")
  utils::install.packages("node", repos = CFG$jamovi_repo)
  need_pkg("node")

  # jmvtools pinned to the exact tarball. repos=NULL => resolves NO deps (node above covers it).
  step(paste0("Installing jmvtools ", CFG$jmvtools_version, " (pinned tarball) ..."))
  utils::install.packages(CFG$jmvtools_tarball, repos = NULL, type = "source")

  need_pkg("jmvtools")
  got <- as.character(utils::packageVersion("jmvtools"))
  if (got != CFG$jmvtools_version) {
    die("jmvtools is ", got, " but MUST be ", CFG$jmvtools_version, ".\n",
        "     The plain repo index serves 28.x; only the pinned tarball is safe.")
  }
  step(paste0("jmvtools ", got, " OK."))
}

# === STAGE 3: package dependencies (for load_all) ===================================

banner("STAGE 3/5  Dependencies")

if (CFG$skip_deps) {
  step("skip_deps=TRUE -> not installing dependencies.")
} else {
  use_pak <- isTRUE(CFG$use_pak) && requireNamespace("pak", quietly = TRUE)
  if (isTRUE(CFG$use_pak) && !use_pak)
    step("pak requested but not installed -> falling back to devtools::install_deps().")

  if (use_pak) {
    # pak stages into a private lib and swaps atomically -> more robust than base install.packages
    # against locked/loaded DLLs, and gives clearer errors. Installs the hard deps (Imports/LinkingTo).
    step("pak::local_install_deps() ...")
    pak::local_install_deps(root = clone_dir, upgrade = FALSE, ask = FALSE)
  } else {
    step(paste0("install_deps(dependencies = ", format(CFG$dep_which), ") ..."))
    devtools::install_deps(clone_dir, dependencies = CFG$dep_which,
                           repos = CFG$cran_repo, upgrade = "never")
  }

  # jmvcore (and any EXTRA_DEPS) are Suggests but needed by the jamovi backend at build/run.
  missing_extra <- CFG$extra_deps[!vapply(CFG$extra_deps, requireNamespace,
                                          logical(1), quietly = TRUE)]
  if (length(missing_extra)) {
    step(paste0("Installing extra deps: ", paste(missing_extra, collapse = ", ")))
    if (use_pak) pak::pkg_install(missing_extra, upgrade = FALSE, ask = FALSE)
    else         utils::install.packages(missing_extra, repos = CFG$cran_repo)
  }
  step("Dependencies ready.")
}

# === STAGE 4: build the .jmo ========================================================

banner("STAGE 4/5  Build (load_all -> jmvtools::install -> load_all)")

# The load-bearing gotcha: unset ELECTRON_RUN_AS_NODE before install() or jamovi's Electron
# runs as plain node and install() dies with rc=9 "bad option: --install".
Sys.unsetenv("ELECTRON_RUN_AS_NODE")
options(jamovi_home = jamovi_home)  # jmvtools also reads getOption("jamovi_home")

step("devtools::load_all() ...")
devtools::load_all(clone_dir, quiet = TRUE)

# Friendly pre-flight. A green check() does NOT guarantee install() succeeds (it never reaches
# Electron) -- so we only log it, never gate on it.
check_out <- tryCatch(utils::capture.output(jmvtools::check(home = jamovi_home)),
                      error = function(e) paste("check() errored:", conditionMessage(e)))
step(paste0("jmvtools::check(): ", paste(check_out, collapse = " | ")))

# Pre-install: kill lingering jamovi engines (they lock the module DLLs) and delete the stale
# installed module, so install-from-file has nothing locked to overwrite (the DLL-lock fix). Close
# the jamovi Desktop app too if it is open -- it holds engines that this catches by path.
if (isTRUE(CFG$stop_jamovi))  stop_jamovi_processes(jamovi_home)
if (isTRUE(CFG$clean_module)) remove_installed_module(jamovi_home)

step("jmvtools::install() -- this builds + installs the .jmo (~2 min) ...")
build_ok <- tryCatch({
  jmvtools::install(clone_dir, home = jamovi_home)
  TRUE
}, error = function(e) {
  cat("!!  jmvtools::install() FAILED: ", conditionMessage(e), "\n", sep = "")
  FALSE
})

# Reload after the .h.R is regenerated by install() (the deliberate double load_all).
suppressWarnings(tryCatch(devtools::load_all(clone_dir, quiet = TRUE),
                          error = function(e)
                            cat("!!  second load_all warned: ", conditionMessage(e), "\n",
                                sep = "")))

# === STAGE 5: verify the module actually landed =====================================

banner("STAGE 5/5  Verify install")

# Best-effort scan of the Windows jamovi module roots for the installed tabxplor module.
module_roots <- jamovi_module_roots(jamovi_home)
module_roots <- module_roots[nzchar(module_roots) & dir.exists(module_roots)]

yaml_path <- NULL
for (root in module_roots) {
  cand <- file.path(root, "tabxplor", "jamovi.yaml")
  if (file.exists(cand)) { yaml_path <- cand; break }
}

verify_ok <- FALSE
if (!is.null(yaml_path)) {
  yml <- readLines(yaml_path, warn = FALSE)
  ver <- grep("^version:",  yml, value = TRUE)
  rv  <- grep("^rVersion:", yml, value = TRUE)
  step(paste0("Installed module: ", dirname(yaml_path)))
  if (length(ver)) step(paste0("  ", trimws(ver[1])))
  if (length(rv))  step(paste0("  ", trimws(rv[1])))
  ui_blob <- file.path(dirname(yaml_path), "ui", "jmvtab.js")
  if (file.exists(ui_blob)) step(paste0("  compiled UI blob present: ", ui_blob))
  verify_ok <- TRUE
} else {
  cat("!!  Could not locate an installed tabxplor/jamovi.yaml in the known module roots:\n",
      paste0("!!    ", module_roots, collapse = "\n"), "\n",
      "!!  If install() reported success, the module dir may be elsewhere -- open jamovi and\n",
      "!!  check the module list, or search for jamovi.yaml under your user profile.\n", sep = "")
}

# Also surface any produced .jmo file (handy for sideloading / archiving).
jmo_files <- unique(c(
  Sys.glob(file.path(clone_dir, "*.jmo")),
  Sys.glob(file.path(clone_dir, "build", "*.jmo"))
))
if (length(jmo_files)) step(paste0(".jmo produced: ", paste(jmo_files, collapse = ", ")))

# === Summary + cleanup ==============================================================

banner("RESULT")
overall <- build_ok && verify_ok
cat("  build_ok  : ", build_ok,  "\n",
    "  verify_ok : ", verify_ok, "\n",
    "  OVERALL   : ", if (overall) "PASS" else "FAIL", "\n", sep = "")

# Kill the jamovi engines install() may have left running, so the NEXT build starts with no DLL
# locked (this is the recurring-failure fix: a lingering engine locks the DLL for the following run).
if (isTRUE(CFG$stop_jamovi)) stop_jamovi_processes(jamovi_home)

if (CFG$keep_clone) {
  cat("\n  Temp clone kept at:\n    ", clone_dir, "\n",
      "  (set keep_clone=FALSE in CONFIG to auto-delete it.)\n", sep = "")
} else {
  unlink(clone_dir, recursive = TRUE, force = TRUE)
  cat("\n  Temp clone deleted.\n")
}

# if (!overall) quit(status = 1L, save = "no")
