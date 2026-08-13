#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# tabxplor Phase 19 — unattended overnight driver
#
# One FRESH `claude -p` session per phase. Not --continue, not --resume:
# a new process with no prior transcript is the only way to get a genuinely
# clean context, and it is the reason this is more reliable than asking the
# model to /compact between phases (it cannot — /compact is user-typed only).
#
# The checkpoint is a GIT COMMIT, not the model's own claim of success.
# A phase that produces no commit halts the run.
#
# Usage:
#   dev/night_run/run_phase19.sh                 # run from the next unfinished phase
#   dev/night_run/run_phase19.sh 19c             # start at a given phase
#   dev/night_run/run_phase19.sh 19c 19f         # run an inclusive range
#   DRY_RUN=1 dev/night_run/run_phase19.sh       # print what would run, do nothing
# ---------------------------------------------------------------------------
set -uo pipefail

REPO="/home/dev1/github/tabxplor"
RUNDIR="$REPO/dev/night_run"
LOGDIR="$RUNDIR/logs/$(date +%Y%m%d_%H%M%S)"
STATE="$RUNDIR/.state"
RULES="$RUNDIR/autonomy_rules.md"

# Every phase in dependency order. 19a is already committed.
ALL_PHASES=(19b 19c 19d 19e 19f 19g 19h 19i 19j 19k 19l 19m 19n)

# Per-phase spend ceiling in USD. A phase that blows through this is looping,
# not working; the cap turns a runaway night into a bounded one.
MAX_BUDGET_USD="${MAX_BUDGET_USD:-25}"

# Wall-clock ceiling per phase (seconds). 19f/19j are the structural ones.
PHASE_TIMEOUT="${PHASE_TIMEOUT:-14400}"   # 4 h

# Stall ceiling: if the event stream stops growing for this long, the phase is
# hung, not thinking. Measured from the real runs this replaces: a Bash tool call
# once blocked for 7 h 45 m, and one session died silently at 01:10 and sat dead
# until 04:21 when a human woke up. A total timeout alone does not catch either
# cheaply — it burns the whole 4 h budget before reacting.
STALL_TIMEOUT="${STALL_TIMEOUT:-1800}"    # 30 min of total silence

mkdir -p "$LOGDIR"
cd "$REPO" || exit 1

log() { printf '%s  %s\n' "$(date +%H:%M:%S)" "$*" | tee -a "$LOGDIR/driver.log"; }

die() { log "HALT: $*"; exit 1; }

# --- preflight ------------------------------------------------------------
command -v claude >/dev/null || die "claude CLI not on PATH"
[ -f "$RULES" ]              || die "missing $RULES"

branch=$(git rev-parse --abbrev-ref HEAD)
[ "$branch" = "dev" ] || die "on branch '$branch'; phases must run on 'dev'"

if [ -n "$(git status --porcelain)" ]; then
  die "working tree is dirty. Commit or stash before starting — the driver uses
       'a new commit appeared' as its success signal, and a dirty tree makes that
       signal meaningless. Currently uncommitted: $(git status --porcelain | wc -l) paths."
fi

# --- which phases to run --------------------------------------------------
start="${1:-}"
end="${2:-}"
if [ -z "$start" ] && [ -f "$STATE" ]; then start=$(cat "$STATE"); fi
if [ -z "$start" ]; then start="${ALL_PHASES[0]}"; fi

phases=()
collecting=0
for p in "${ALL_PHASES[@]}"; do
  [ "$p" = "$start" ] && collecting=1
  [ $collecting -eq 1 ] && phases+=("$p")
  [ -n "$end" ] && [ "$p" = "$end" ] && break
done
[ ${#phases[@]} -gt 0 ] || die "no phases selected (start='$start')"

log "run dir     : $LOGDIR"
log "phases      : ${phases[*]}"
log "budget/phase: \$$MAX_BUDGET_USD   timeout/phase: ${PHASE_TIMEOUT}s"
log "head        : $(git log -1 --format='%h %s')"

# --- the per-phase prompt -------------------------------------------------
build_prompt() {
  local phase="$1"
  cat <<EOF
Implement **Phase $phase** of the tabxplor 2.0.0 Phase 19 roadmap, start to finish,
in this single session, unattended.

Read first, in this order:
1. \`dev/tabxplor_phase19_ecosystem_integration.md\` — the plan of plans. Read §1 (the
   mission and the hard rules), §4 (settled decisions — do not re-open), §8
   (verification discipline), and in full the entry \`#### Phase $phase\`.
2. The Phase 19 section of \`CLAUDE.md\` — the big picture and the DONE summaries of the
   phases already landed, so you build on what is actually there rather than on the
   plan's expectation of it.
3. \`dev/tabxplor_architecture.md\` — the section covering each file you are about to
   touch.

Then write your implementation plan as ordinary text — do NOT enter plan mode, there is
nobody awake to approve it — and implement it.

Phase 19's governing rules apply in full, and they override convenience:
- **Simplify and integrate; never add another ad-hoc layer.** Delete the superseded
  implementation in the same phase.
- **Never guess what something is.** No behaviour may depend on a rendered English
  label, a name prefix, a positional vector or a magic field value. If the fact is not
  stored, storing it IS the task.
- **One resolver, one model, taken to completion.** Re-deriving downstream is the disease.
- **Facts live in ONE table.** Two encodings "kept in sync by comment" is forbidden.
- **Never leave a representation half-migrated.** Split the session, never the migration.
- \`tab_reg()\`'s back-compatibility is waived entirely, user API included. \`tab()\`'s
  CRAN-released surface gets soft-deprecation shims, never silent breakage.
- **A claimed fix ships with the fixture that fails without it.**

Where you see a more integrated, consistent, future-proof way to reach the same end,
and it is consistent with the phase's stated goal, take it — that is the point of this
phase. Where you see a caveat, an inconsistency, or a white elephant in the plan, do not
silently work around it: implement the best version you can and record it under OPEN
QUESTIONS in your report.

Finish by satisfying every item in the "What done means for a phase" checklist you were
given, including the commit and the report.
EOF
}

# --- run ------------------------------------------------------------------
for phase in "${phases[@]}"; do
  before=$(git rev-parse HEAD)
  plog="$LOGDIR/$phase.jsonl"
  rlog="$LOGDIR/$phase.result.json"

  log "=== Phase $phase : starting (head $(git rev-parse --short HEAD)) ==="

  if [ "${DRY_RUN:-0}" = "1" ]; then
    build_prompt "$phase" > "$LOGDIR/$phase.prompt.txt"
    log "DRY_RUN: prompt written to $LOGDIR/$phase.prompt.txt"
    continue
  fi

  # A fresh session: no --continue, no --resume, no --session-id reuse.
  # bypassPermissions must be passed EVERY invocation — it is never restored.
  # The autonomy rules go in the system prompt so auto-compaction cannot eat them.
  timeout --signal=TERM "$PHASE_TIMEOUT" \
    claude -p "$(build_prompt "$phase")" \
      --append-system-prompt-file "$RULES" \
      --permission-mode bypassPermissions \
      --model opus \
      --effort high \
      --max-budget-usd "$MAX_BUDGET_USD" \
      --output-format stream-json \
      --include-partial-messages \
      --verbose \
      >"$plog" 2>"$LOGDIR/$phase.stderr" &
  claude_pid=$!

  # Stall watchdog: kill the phase if the event stream goes silent.
  ( last_size=-1; quiet=0
    while kill -0 "$claude_pid" 2>/dev/null; do
      sleep 60
      size=$(stat -c %s "$plog" 2>/dev/null || echo 0)
      if [ "$size" = "$last_size" ]; then
        quiet=$((quiet + 60))
        if [ "$quiet" -ge "$STALL_TIMEOUT" ]; then
          echo "STALLED: no output for ${quiet}s, killing $claude_pid" \
            >> "$LOGDIR/driver.log"
          kill -TERM "$claude_pid" 2>/dev/null
          sleep 20; kill -KILL "$claude_pid" 2>/dev/null
          exit 0
        fi
      else
        quiet=0; last_size=$size
      fi
    done ) &
  watchdog_pid=$!

  wait "$claude_pid"; rc=$?
  kill "$watchdog_pid" 2>/dev/null; wait "$watchdog_pid" 2>/dev/null

  # The final stream-json line is the result envelope.
  tail -n 200 "$plog" | grep '"type":"result"' | tail -n 1 > "$rlog" || true

  cost=$(python3 -c 'import json,sys
try: print(f"{json.load(open(sys.argv[1])).get(\"total_cost_usd\",0):.2f}")
except Exception: print("?")' "$rlog" 2>/dev/null)
  sid=$(python3 -c 'import json,sys
try: print(json.load(open(sys.argv[1])).get("session_id","?"))
except Exception: print("?")' "$rlog" 2>/dev/null)

  after=$(git rev-parse HEAD)
  log "Phase $phase : rc=$rc cost=\$$cost session=$sid"

  # --- rate-limit / auth stop, distinguished from a real failure -----------
  if grep -qiE 'usage limit|rate.?limit|Claude AI usage limit reached|quota' "$rlog" "$LOGDIR/$phase.stderr" 2>/dev/null; then
    echo "$phase" > "$STATE"
    die "usage limit reached during $phase. State saved: resume with
         dev/night_run/run_phase19.sh $phase"
  fi

  if [ $rc -eq 124 ] || [ $rc -eq 143 ]; then
    echo "$phase" > "$STATE"
    die "phase $phase exceeded ${PHASE_TIMEOUT}s and was killed. Inspect $plog"
  fi

  # --- the real success test: did a commit land? --------------------------
  if [ "$before" = "$after" ]; then
    echo "$phase" > "$STATE"
    die "phase $phase produced NO commit. Refusing to start the next phase on an
         unverified foundation. Inspect $plog and 'git status'."
  fi

  if [ -n "$(git status --porcelain)" ]; then
    log "WARNING: phase $phase committed but left the tree dirty:"
    git status --porcelain | head -20 | tee -a "$LOGDIR/driver.log"
  fi

  log "=== Phase $phase : DONE -> $(git log -1 --format='%h %s') ==="

  # Next phase becomes the resume point.
  nxt=""
  seen=0
  for p in "${ALL_PHASES[@]}"; do
    [ $seen -eq 1 ] && { nxt="$p"; break; }
    [ "$p" = "$phase" ] && seen=1
  done
  [ -n "$nxt" ] && echo "$nxt" > "$STATE" || rm -f "$STATE"
done

log "ALL SELECTED PHASES COMPLETE"
log "commits this run:"
git log --oneline --format='  %h %ad %s' --date=short "$(git rev-parse HEAD)" \
  ^"$(git rev-parse HEAD~${#phases[@]} 2>/dev/null || echo HEAD)" 2>/dev/null \
  | tee -a "$LOGDIR/driver.log"
