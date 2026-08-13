#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# tabxplor night run — build the per-phase prompt files
#
# One prompt file per phase, each assembled as
#
#     <PREFIX> <the exact phase header>.
#     <SUFFIX>
#
# The phase headers are READ FROM THE ROADMAP, never typed here: the roadmap is
# the single source of truth for which phases exist, their order, and their exact
# titles. That matters because the phase session is told to use "the exact phase
# header, and nothing else" as its commit message, and the driver reads that
# commit back as the success signal. One transcription slip would break the loop.
#
# The standard night-run rules are NOT in these prompts. They go in rules.md,
# passed as a system prompt, where auto-compaction cannot reach them.
#
# Usage:
#   build_prompts.sh --from 19d --to 19k --prefix "…" --suffix "…"
#   build_prompts.sh --phases "19d 19e 19g" --prefix-file p.txt --suffix-file s.txt
#   build_prompts.sh --list                      # show the phases the roadmap declares
#
# Options:
#   --from <id>          first phase (inclusive), in roadmap order
#   --to <id>            last phase (inclusive); defaults to the last declared
#   --phases "<a b c>"   explicit list instead of a range
#   --prefix <text>      common prefix, verbatim
#   --suffix <text>      common suffix, verbatim
#   --prefix-file <f>    read the prefix from a file instead
#   --suffix-file <f>    read the suffix from a file instead
#   --roadmap <f>        roadmap to read headers from
#                        (default dev/tabxplor_phase19_ecosystem_integration.md)
#   --list               list declared phases and exit
# ---------------------------------------------------------------------------
set -uo pipefail

# Overridable so the driver can be rehearsed against a scratch repo.
REPO="${NIGHT_RUN_REPO:-/home/dev1/github/tabxplor}"
RUNDIR="${NIGHT_RUN_DIR:-$REPO/dev/night_run}"
PROMPTS="$RUNDIR/prompts"
ROADMAP="${NIGHT_RUN_ROADMAP:-$REPO/dev/tabxplor_phase19_ecosystem_integration.md}"

FROM=""; TO=""; PHASES=""; PREFIX=""; SUFFIX=""; LIST=0

die() { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

while [ $# -gt 0 ]; do
  case "$1" in
    --from)        FROM="$2"; shift 2 ;;
    --to)          TO="$2"; shift 2 ;;
    --phases)      PHASES="$2"; shift 2 ;;
    --prefix)      PREFIX="$2"; shift 2 ;;
    --suffix)      SUFFIX="$2"; shift 2 ;;
    --prefix-file) PREFIX="$(cat "$2")" || die "cannot read $2"; shift 2 ;;
    --suffix-file) SUFFIX="$(cat "$2")" || die "cannot read $2"; shift 2 ;;
    --roadmap)     ROADMAP="$2"; shift 2 ;;
    --list)        LIST=1; shift ;;
    -h|--help)     sed -n '2,40p' "$0"; exit 0 ;;
    *)             die "unknown option: $1" ;;
  esac
done

[ -f "$ROADMAP" ] || die "roadmap not found: $ROADMAP"

# --- read the declared phases --------------------------------------------
# `#### Phase 19d — KEY 8a: the `tab()` comparison surface`
#    -> id "19d", header "Phase 19d — KEY 8a: the `tab()` comparison surface"
mapfile -t HEADERS < <(grep -E '^#### Phase [0-9]+[a-z]* — ' "$ROADMAP" | sed 's/^#### //')
[ ${#HEADERS[@]} -gt 0 ] || die "no '#### Phase <id> — <title>' headings in $ROADMAP"

declare -a IDS=()
declare -A HEADER_OF=()
for h in "${HEADERS[@]}"; do
  id=$(printf '%s' "$h" | sed -E 's/^Phase ([0-9]+[a-z]*) — .*/\1/')
  if [ -n "${HEADER_OF[$id]+x}" ]; then
    printf 'WARNING: phase %s declared twice in the roadmap; keeping the first\n' "$id" >&2
    continue
  fi
  IDS+=("$id"); HEADER_OF[$id]="$h"
done

if [ "$LIST" = 1 ]; then
  printf 'Phases declared in %s:\n' "${ROADMAP#$REPO/}"
  for id in "${IDS[@]}"; do printf '  %-5s %s\n' "$id" "${HEADER_OF[$id]}"; done
  exit 0
fi

# --- select tonight's phases ----------------------------------------------
declare -a SELECTED=()
if [ -n "$PHASES" ]; then
  for id in $PHASES; do
    [ -n "${HEADER_OF[$id]+x}" ] || die "phase '$id' is not declared in the roadmap (--list to see them)"
    SELECTED+=("$id")
  done
else
  [ -n "$FROM" ] || die "give --from (and optionally --to), or --phases"
  [ -n "${HEADER_OF[$FROM]+x}" ] || die "phase '$FROM' is not declared in the roadmap (--list to see them)"
  [ -z "$TO" ] || [ -n "${HEADER_OF[$TO]+x}" ] || die "phase '$TO' is not declared in the roadmap"
  collecting=0
  for id in "${IDS[@]}"; do
    [ "$id" = "$FROM" ] && collecting=1
    [ $collecting -eq 1 ] && SELECTED+=("$id")
    [ -n "$TO" ] && [ "$id" = "$TO" ] && break
  done
  if [ -n "$TO" ]; then
    last="${SELECTED[${#SELECTED[@]}-1]}"
    [ "$last" = "$TO" ] || die "'$TO' comes before '$FROM' in the roadmap; check the order"
  fi
fi
[ ${#SELECTED[@]} -gt 0 ] || die "no phases selected"

[ -n "$PREFIX" ] || die "give --prefix or --prefix-file"
[ -n "$SUFFIX" ] || die "give --suffix or --suffix-file"

# --- write ----------------------------------------------------------------
rm -rf "$PROMPTS"; mkdir -p "$PROMPTS"

for id in "${SELECTED[@]}"; do
  { printf '%s **%s**.\n\n' "${PREFIX%$'\n'}" "${HEADER_OF[$id]}"
    printf '%s\n' "$SUFFIX"
  } > "$PROMPTS/$id.txt"
done

printf '%s\n' "${SELECTED[@]}" > "$PROMPTS/PHASES"

printf 'Wrote %d prompt(s) to %s\n\n' "${#SELECTED[@]}" "${PROMPTS#$REPO/}"
for id in "${SELECTED[@]}"; do
  printf '  %-5s %-4s lines   %s\n' "$id" "$(wc -l < "$PROMPTS/$id.txt")" "${HEADER_OF[$id]}"
done

# --- flag phases that are a poor fit for unattended work ------------------
# Data-driven, from the roadmap's own words — never a hand-kept list here.
# Two sources, because the caveats are split across them: the phase's body in the
# plan-of-plans, and its row in CLAUDE.md's condensed phase table (which is where
# "(creative, ask before building)" and "Abandon rather than force" actually live).
warned=0
for id in "${SELECTED[@]}"; do
  body=$(awk -v pat="^#### Phase $id — " '
    $0 ~ pat {inside=1; next}
    /^#### Phase [0-9]+[a-z]* — / {inside=0}
    inside {print}' "$ROADMAP")
  row=$(grep -F "**$id**" "$REPO/CLAUDE.md" 2>/dev/null)
  text="${HEADER_OF[$id]}"$'\n'"$body"$'\n'"$row"

  reason=""
  printf '%s' "$text" | grep -qiE 'creative|ask before building|discuss(ed)? first' \
    && reason="called creative / to be discussed first"
  printf '%s' "$text" | grep -qiE 'abandon rather than force|half-migrated|split the \*?session' \
    && reason="${reason:+$reason; }must not be interrupted mid-flight"
  if [ -n "$reason" ]; then
    [ $warned -eq 0 ] && printf '\nPoor fit for an unattended run:\n' && warned=1
    printf '  %-5s %s\n' "$id" "$reason"
  fi
done
if [ $warned -eq 1 ]; then
  printf '  -> consider running these awake, or trimming the range.\n'
  printf '     A watchdog kill mid-migration leaves exactly the half-migrated state\n'
  printf '     the plan forbids.\n'
fi

printf '\nNext: %s\n' "dev/night_run/run_night.sh"
exit 0
