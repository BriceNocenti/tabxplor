# PURPOSE: prove that a freshly built .jmo is a file a student can actually sideload.
# ROLE: the one verifier behind `.github/workflows/jmo.yaml`, run by JAMOVI'S OWN R on the system
#   that built the file -- macOS, Windows and Linux alike -- so the seven jobs assert the same
#   things in one language instead of three shells. It replaces the "is the file big enough?" check
#   the public precedents use, because the compiler never loads a macOS binary it has patched:
#   until this script, a green build proved only that a file had been written.
# KEY CONSTRAINTS:
#   - It reads the UNZIPPED .jmo, never the build directory: the artefact is what ships.
#   - `install.packages()` reports a failed SOURCE build as a warning, so the compiler can finish
#     and write a valid .jmo with a package simply missing. `REQUIRED` is what catches that. ⚠ It is
#     checked by LOADING, not by looking in the module's directory: the compiler deliberately omits
#     whatever jamovi's own library already holds, and which packages those are differs per jamovi
#     line -- `fansi` ships with the 2.7 line and not with 28. What the module owes a user is that
#     the package is THERE when R asks for it, wherever it comes from.
#   - `.libPaths()` is pinned to the module plus jamovi's own libraries -- the pair the compiler
#     itself computes -- and the pin is then asserted. A second R library on the same R minor
#     version otherwise wins silently.
#   - macOS load commands are rewritten by the compiler to `@executable_path/../Frameworks/...`,
#     which resolves against JAMOVI's executable, not R's. `check_macho()` resolves every one of
#     them against the app: it is the STATIC half, and the one that answers the question a
#     student's Mac asks. `check_load()` only works because the workflow symlinks a stand-in for
#     jamovi's executable, so it proves the code runs, never that the rewrite is right.
# See: dev/jamovi_library_vs_sideloading.md section 3 (why seven files, and what jamovi checks).

# DESIGN: two switches, because both rest on something unproven on a CI runner rather than on a
# supported mechanism -- the stand-in symlink, and whether jmvcore instantiates an analysis with no
# jamovi server around it. Flipping one keeps the rest of the verification.
MACOS_LOAD_IS_FATAL <- TRUE
ANALYSIS_IS_FATAL <- TRUE

# What a usable module must be able to LOAD. Some of these the compiler vendors, some jamovi's own
# library already holds; openxlsx2 is the one built from source, and the one this list exists for.
REQUIRED <- c("tabxplor", "openxlsx2", "survey", "VGAM", "svyVGAM", "brant",
              "marginaleffects", "mirai", "nanonext", "parallelly", "RhpcBLASctl", "fansi")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 5L) {
  stop("usage: jmo-verify.R <file.jmo> <jamovi home | 'flatpak'> <platform> <expected R> <expected version>")
}
jmo <- args[[1]]; home <- args[[2]]; platform <- args[[3]]
expect_r <- args[[4]]; expect_version <- args[[5]]

os <- if (grepl("^macos", platform)) "macos" else if (grepl("^win", platform)) "win" else "linux"
arch <- if (grepl("arm64$", platform)) "arm64" else "x64"

failures <- character()
fail <- function(title, ...) {
  msg <- paste0(...)
  failures <<- c(failures, msg)
  cat("::error title=", title, "::", msg, "\n", sep = "")
}
note <- function(...) cat("::notice::", paste0(...), "\n", sep = "")
say  <- function(...) cat(paste0(...), "\n", sep = "")

# --- the artefact --------------------------------------------------------------------------------

if (file.size(jmo) < 5e6) {
  # a compiler that installed nothing still writes a valid, tiny zip
  fail("Suspiciously small", basename(jmo), " is only ",
       format(file.size(jmo) / 1024^2, digits = 3), " MB")
}
mod <- file.path(tempdir(), "jmo")
unlink(mod, recursive = TRUE); dir.create(mod, recursive = TRUE)
utils::unzip(jmo, exdir = mod)
root <- file.path(mod, "tabxplor")
if (!dir.exists(root)) stop("the .jmo holds no 'tabxplor/' directory: ", jmo)
rlib <- file.path(root, "R")

# The yaml package need not be in jamovi's library, and three scalar fields do not need it.
yaml_field <- function(path, key) {
  hit <- grep(paste0("^", key, ":"), readLines(path, warn = FALSE), value = TRUE)
  if (!length(hit)) return(NA_character_)
  trimws(gsub("['\"]", "", sub(paste0("^", key, ":"), "", hit[[1]])))
}
dcf_field <- function(path, key) unname(read.dcf(path, fields = key)[1, 1])

check_stamp <- function() {
  y <- file.path(root, "jamovi.yaml")
  want <- paste0(expect_r, "-", arch)
  got <- yaml_field(y, "rVersion")
  if (!identical(got, want)) {
    fail("Stamp does not match the grid",
         "rVersion is '", got, "', the grid declares '", want,
         "'. jamovi compares this string exactly, so this file would be refused as incompatible.")
  } else say("rVersion   ", got, "   OK")

  running <- paste0(as.character(getRversion()), "-",
                    switch(R.version$arch, aarch64 = "arm64", x86_64 = "x64", R.version$arch))
  if (!identical(got, running)) {
    fail("Stamp is not this R", "the .jmo says ", got, " but the R verifying it is ", running,
         " -- --home pointed at a different jamovi than the one running this script")
  }
}

# The module version is stated once, in DESCRIPTION, and must arrive intact in both other places.
check_version <- function() {
  got <- c(jamovi.yaml = yaml_field(file.path(root, "jamovi.yaml"), "version"),
           installed = dcf_field(file.path(rlib, "tabxplor", "DESCRIPTION"), "Version"))
  bad <- got[got != expect_version]
  if (length(bad)) {
    fail("Module version drifted",
         "DESCRIPTION says '", expect_version, "', but ",
         paste0(names(bad), " says '", bad, "'", collapse = "; "))
  } else say("version    ", expect_version, "   OK, in jamovi.yaml and in the installed package")
}

check_library <- function() {
  pkgs <- list.dirs(rlib, recursive = FALSE, full.names = FALSE)
  if (!"tabxplor" %in% pkgs) fail("tabxplor is missing", "the module's R library holds no tabxplor")
  say("vendored   ", length(pkgs), " packages: ", paste(pkgs, collapse = ", "))
  pkgs
}

# --- macOS: every load command must resolve on a machine that has only jamovi ---------------------

check_macho <- function() {
  if (os != "macos") return(invisible())
  libs <- list.files(rlib, pattern = "\\.(so|dylib)$", recursive = TRUE, full.names = TRUE)
  say("otool      ", length(libs), " compiled objects")
  exe_dir <- file.path(home, "Contents", "MacOS")   # what @executable_path means inside jamovi
  for (lib in libs) {
    out <- suppressWarnings(system2("otool", c("-L", lib), stdout = TRUE, stderr = TRUE))
    deps <- trimws(sub("\\s*\\(compatibility.*$", "", out[-1]))
    deps <- deps[nzchar(deps) & basename(deps) != basename(lib)]
    for (d in deps) {
      if (grepl("^(/usr/lib/|/System/)", d)) next
      if (grepl("^@executable_path/", d)) {
        if (!file.exists(file.path(exe_dir, sub("^@executable_path/", "", d)))) {
          fail("Load command does not resolve",
               basename(lib), " asks for '", d, "', which does not exist inside this jamovi")
        }
        next
      }
      if (grepl("^@loader_path/", d)) {
        if (!file.exists(file.path(dirname(lib), sub("^@loader_path/", "", d)))) {
          fail("Load command does not resolve", basename(lib), " asks for '", d, "'")
        }
        next
      }
      if (grepl("^@rpath/", d)) { note(basename(lib), " uses ", d); next }
      fail("Load command points outside jamovi",
           basename(lib), " links '", d, "', a path no student's machine has. The compiler ",
           "rewrites only a fixed list of library names; this one escaped it.")
    }
  }
}

# --- loading it, the way jamovi will --------------------------------------------------------------

jamovi_libs <- function() {
  if (home == "flatpak") return("/app/lib/jamovi/modules/jmv/R")  # /app/lib/R/library is .Library
  base <- if (os == "macos") file.path(home, "Contents") else home
  file.path(base, "Resources", "modules", c("base", "jmv"), "R")
}

check_load <- function(pkgs) {
  want <- c(rlib, Filter(dir.exists, jamovi_libs()))
  .libPaths(want)
  stray <- setdiff(normalizePath(.libPaths()), normalizePath(c(want, .Library)))
  if (length(stray)) {
    fail("Foreign R library on the path", paste(stray, collapse = ", "),
         " -- a second library silently answering for jamovi's own is how a version skew hides")
  }
  say("libPaths   ", paste(.libPaths(), collapse = "\n           "))

  for (p in setdiff(pkgs, "tabxplor")) {
    ok <- tryCatch({ loadNamespace(p); TRUE }, error = function(e) FALSE)
    if (!ok) {
      m <- paste0("'", p, "' does not load in jamovi's R")
      if (os == "macos") {
        m <- paste0(m, " -- if its .so passed the otool check, suspect the @executable_path ",
                    "stand-in symlink the workflow creates, not the build")
      }
      if (os != "macos" || MACOS_LOAD_IS_FATAL) fail("Package does not load", m) else note(m)
    }
  }
  # the REQUIRED contract: available to R once the paths are jamovi's, vendored or bundled
  gone <- REQUIRED[!vapply(REQUIRED, requireNamespace, TRUE, quietly = TRUE)]
  if (length(gone)) {
    fail("Required package cannot be loaded",
         paste(gone, collapse = ", "), " is neither vendored in the module nor in jamovi's own ",
         "library. A source build that failed is reported by install.packages() as a warning, so ",
         "the .jmo can look complete without it.")
  }
  library(tabxplor)
  say("loaded     tabxplor ", as.character(utils::packageVersion("tabxplor")))
}

# --- and running it -------------------------------------------------------------------------------

check_run <- function() {
  d <- tabxplor::questionr_hdv
  step <- function(label, expr, fatal = TRUE) {
    ok <- tryCatch({ force(expr); TRUE },
                   error = function(e) {
                     m <- paste0(label, " failed: ", conditionMessage(e))
                     if (fatal) fail("Smoke test failed", m) else note(m)
                     FALSE
                   })
    if (ok) say("ran        ", label)
  }
  tb <- tab(d, sexe, cinema, wt = poids, pct = "row", color = "diff", ci = "cell", test = TRUE)
  step("tab()", tb)
  step("tab_reg()", tab_reg(d, cinema, c(sexe, qualif, age)))
  step("tab_html()", tab_html(tb))
  step("tab_md()", tab_md(tb, print = FALSE))   # print = NULL cats in a console session
  # the only exercise of openxlsx2, which the 28 line compiles from source
  step("tab_xl()", tab_xl(tb, path = tempfile(fileext = ".xlsx")))
  # the panel path end to end: what would have caught a generated option with no default
  if (requireNamespace("jmvcore", quietly = TRUE)) {
    step("jmvtab()",
         # the panel's own vocabulary: its values are the full measure names, never the acronyms
         jmvtab(data = as.data.frame(d), row_vars = "sexe", col_vars = "cinema",
                pct = "row", color = "difference"),
         fatal = ANALYSIS_IS_FATAL)
  } else {
    note("jmvcore is not in this jamovi's library; the jmvtab() path was not exercised")
  }
}

# --- report ----------------------------------------------------------------------------------------

say("jmo        ", basename(jmo), "   (", format(file.size(jmo) / 1024^2, digits = 3), " MB)")
check_stamp()
if (length(failures)) { say("\nthe stamp is wrong; nothing else is worth checking."); quit(status = 1L) }
check_version()
pkgs <- check_library()
check_macho()
check_load(pkgs)
check_run()

if (length(failures)) {
  say("\n", length(failures), " check(s) failed.")
  quit(status = 1L)
}
say("\nall checks passed.")
