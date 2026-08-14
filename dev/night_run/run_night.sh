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
#   dev/night_run/run_night.sh                 # run from the next unfinished phase
#   dev/night_run/run_night.sh 19c             # start at a given phase
#   dev/night_run/run_night.sh 19c 19f         # run an inclusive range
#   DRY_RUN=1 dev/night_run/run_night.sh       # print what would run, do nothing
# ---------------------------------------------------------------------------
set -uo pipefail

# Overridable so the driver can be rehearsed against a scratch repo.
REPO="${NIGHT_RUN_REPO:-/home/dev1/github/tabxplor}"
RUNDIR="${NIGHT_RUN_DIR:-$REPO/dev/night_run}"
LOGDIR="$RUNDIR/logs/$(date +%Y%m%d_%H%M%S)"
STATE="$RUNDIR/.state"
RULES="$RUNDIR/rules.md"
PROMPTS="$RUNDIR/prompts"

# Tonight's phases come from the manifest build_prompts.sh wrote beside the
# prompts. Nothing about the run is hand-maintained in two places.
ALL_PHASES=()
declare -A ATTEMPTS=()

# The real "should I start another phase now" question is not money — it is the
# 5-hour session window. Start a phase with too little left and it gets cut off
# mid-migration; wait for the reset and it runs whole. `/usage` answers this for
# free (handled locally by the CLI: zero tokens, zero turns).
#
# MIN_SESSION_PCT: refuse to start a phase unless this much of the window is left.
# The driver also LEARNS: it records what each completed phase actually consumed
# and requires 1.3x the worst observed, so the estimate improves as the night goes.
MIN_SESSION_PCT="${MIN_SESSION_PCT:-45}"
# Weekly cap is not something a wait can fix; halt and say when it lifts.
WEEK_HALT_PCT="${WEEK_HALT_PCT:-98}"
# Attempts per phase before giving up (a phase that commits "— partial" is retried).
MAX_ATTEMPTS="${MAX_ATTEMPTS:-3}"

# Wall-clock ceiling per phase (seconds). 19f/19j are the structural ones.
PHASE_TIMEOUT="${PHASE_TIMEOUT:-14400}"   # 4 h

# Stall ceiling: if the event stream stops growing for this long, the phase is
# hung, not thinking. Measured from the real runs this replaces: a Bash tool call
# once blocked for 7 h 45 m, and one session died silently at 01:10 and sat dead
# until 04:21 when a human woke up. A total timeout alone does not catch either
# cheaply — it burns the whole 4 h timeout before reacting.
STALL_TIMEOUT="${STALL_TIMEOUT:-1800}"    # 30 min of total silence
STALL_POLL="${STALL_POLL:-60}"            # how often the watchdog samples (test hook)

# Grace between SIGTERM and SIGKILL. `timeout` alone never escalates, and bash does
# not interrupt a foreground child on SIGTERM, so without this a stuck phase survives
# its own timeout. Measured: a hung phase ignored SIGTERM entirely.
KILL_AFTER="${KILL_AFTER:-60}"

mkdir -p "$LOGDIR"
cd "$REPO" || exit 1

log() { printf '%s  %s\n' "$(date +%H:%M:%S)" "$*" | tee -a "$LOGDIR/driver.log"; }

die() { log "HALT: $*"; exit 1; }

# --- preflight ------------------------------------------------------------
command -v claude >/dev/null || die "claude CLI not on PATH"
[ -f "$RULES" ]              || die "missing $RULES"
[ -f "$PROMPTS/PHASES" ]     || die "no prompts built. Run build_prompts.sh first —
       it writes prompts/<phase>.txt and the PHASES manifest this driver reads."

mapfile -t ALL_PHASES < "$PROMPTS/PHASES"
[ ${#ALL_PHASES[@]} -gt 0 ] || die "$PROMPTS/PHASES is empty"

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
log "window gate : need ${MIN_SESSION_PCT}% of the 5h session   timeout/phase: ${PHASE_TIMEOUT}s"
RUN_BASE=$(git rev-parse HEAD)
log "head        : $(git log -1 --format='%h %s')"

# --- the per-phase prompt -------------------------------------------------
# Prompts are FILES, written by build_prompts.sh, so the maintainer can read
# exactly what each phase will be told before going to bed.
build_prompt() { cat "$PROMPTS/$1.txt"; }

# Checked BEFORE any output redirection: a die() inside build_prompt would have
# its message swallowed into the redirect target and exit silently.
for phase in "${phases[@]}"; do
  [ -f "$PROMPTS/$phase.txt" ] || die "missing prompt file $PROMPTS/$phase.txt —
       run the preparation session first (see dev/night_run_prompt.md)"
done

# --- the 5-hour window gate -----------------------------------------------
# Returns only when there is room to run a whole phase, or dies if there never
# will be tonight. Costs nothing: /usage is answered locally.
worst_pct=0          # worst per-phase consumption observed this run
usage_snapshot() {   # -> "<session%> <week%> <session_reset> <week_reset> <human>"
  (cd /tmp && timeout 120 claude -p "/usage" --output-format json 2>/dev/null) \
    | python3 "$RUNDIR/parse_usage.py" 2>>"$LOGDIR/driver.log"
}

wait_for_window() {
  local phase="$1" tries=0
  while :; do
    local snap spct wpct sreset wreset human need
    if ! snap=$(usage_snapshot); then
      log "WARNING: could not read /usage; proceeding without the window gate"
      return 0
    fi
    read -r spct wpct sreset wreset human <<< "$snap"

    if [ "$wpct" -ge "$WEEK_HALT_PCT" ]; then
      echo "$phase" > "$STATE"
      die "weekly limit is at ${wpct}% — a 5h reset will not help.
       It lifts $(date -d "@$wreset" '+%a %d %b %H:%M' 2>/dev/null || echo "$wreset").
       Resume then with: dev/night_run/run_night.sh $phase"
    fi

    need="$MIN_SESSION_PCT"
    if [ "$worst_pct" -gt 0 ]; then
      local learned=$(( worst_pct * 13 / 10 ))
      [ "$learned" -gt "$need" ] && need="$learned"
    fi
    local left=$(( 100 - spct ))
    if [ "$left" -ge "$need" ]; then
      log "window OK for $phase: ${left}% of session left (need ${need}%), week ${wpct}%"
      return 0
    fi

    tries=$((tries + 1))
    if [ "$tries" -gt 3 ] || [ "$sreset" -le 0 ]; then
      echo "$phase" > "$STATE"
      die "only ${left}% of the 5h session left (need ${need}%) and the reset time
       is unusable. Resume with: dev/night_run/run_night.sh $phase"
    fi
    local secs=$(( sreset - $(date +%s) + 90 ))
    [ "$secs" -lt 60 ] && secs=60
    log "HOLDING $phase: ${left}% of session left, need ${need}%. Sleeping ${secs}s
       until the window resets at ${human}."
    sleep "$secs"
  done
}

# --- run ------------------------------------------------------------------
pi=0
while [ "$pi" -lt "${#phases[@]}" ]; do
  phase="${phases[$pi]}"
  attempt=$(( ${ATTEMPTS[$phase]:-0} + 1 )); ATTEMPTS[$phase]=$attempt
  before=$(git rev-parse HEAD)
  [ "$attempt" -gt 1 ] && log "=== Phase $phase : ATTEMPT $attempt of $MAX_ATTEMPTS ==="
  wait_for_window "$phase"
  read -r pre_pct _ _ _ _ <<< "$(usage_snapshot || echo '0 0 0 0 x')"
  plog="$LOGDIR/$phase.a$attempt.jsonl"; rlog="$LOGDIR/$phase.a$attempt.result.json"
  log "=== Phase $phase : starting (head $(git rev-parse --short HEAD)) ==="

  if [ "${DRY_RUN:-0}" = "1" ]; then
    build_prompt "$phase" > "$LOGDIR/$phase.prompt.txt"
    log "DRY_RUN: prompt written to $LOGDIR/$phase.prompt.txt"
    pi=$((pi + 1)); continue
  fi

  # A fresh session: no --continue, no --resume, no --session-id reuse.
  # bypassPermissions must be passed EVERY invocation — it is never restored.
  # The autonomy rules go in the system prompt so auto-compaction cannot eat them.
  timeout --signal=TERM --kill-after="$KILL_AFTER" "$PHASE_TIMEOUT" \
    claude -p "$(build_prompt "$phase")" \
      --append-system-prompt-file "$RULES" \
      --permission-mode bypassPermissions \
      --model opus \
      --effort high \
      --output-format stream-json \
      --include-partial-messages \
      --verbose \
      >"$plog" 2>"$LOGDIR/$phase.a$attempt.stderr" &
  claude_pid=$!

  # Stall watchdog: kill the phase if the event stream goes silent.
  ( last_size=-1; quiet=0
    while kill -0 "$claude_pid" 2>/dev/null; do
      for _ in $(seq "$STALL_POLL"); do
        sleep 1; kill -0 "$claude_pid" 2>/dev/null || exit 0
      done
      size=$(stat -c %s "$plog" 2>/dev/null || echo 0)
      if [ "$size" = "$last_size" ]; then
        quiet=$((quiet + STALL_POLL))
        if [ "$quiet" -ge "$STALL_TIMEOUT" ]; then
          touch "$LOGDIR/$phase.a$attempt.stalled"
          echo "STALLED: no output for ${quiet}s, killing $claude_pid" \
            | tee -a "$LOGDIR/driver.log"
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

  if parsed=$(python3 "$RUNDIR/parse_result.py" "$rlog" 2>>"$LOGDIR/driver.log"); then
    read -r cost sid c1h c5m <<< "$parsed"
    parse_ok=1
  else
    cost="?"; sid="?"; c1h="?"; c5m="?"; parse_ok=0
  fi

  read -r post_pct _ _ _ _ <<< "$(usage_snapshot || echo '0 0 0 0 x')"
  if [ "${post_pct:-0}" -gt "${pre_pct:-0}" ]; then
    used=$(( post_pct - pre_pct ))
    [ "$used" -gt "$worst_pct" ] && worst_pct=$used
    log "Phase $phase consumed ~${used}% of the 5h window (worst so far ${worst_pct}%)"
  fi
  after=$(git rev-parse HEAD)
  log "Phase $phase : rc=$rc cost≈\$$cost session=$sid cache(1h/5m)=$c1h/$c5m"

  # --- rate-limit / auth stop, distinguished from a real failure -----------
  if grep -qiE 'usage limit|rate.?limit|Claude AI usage limit reached|quota' "$rlog" "$LOGDIR/$phase.a$attempt.stderr" 2>/dev/null; then
    echo "$phase" > "$STATE"
    die "usage limit reached during $phase. State saved: resume with
         dev/night_run/run_night.sh $phase"
  fi

  if [ -f "$LOGDIR/$phase.a$attempt.stalled" ]; then
    echo "$phase" > "$STATE"
    die "phase $phase STALLED: no output for ${STALL_TIMEOUT}s, so it was killed.
       Inspect $plog, then resume with:
         dev/night_run/run_night.sh $phase"
  fi

  if [ $rc -eq 124 ] || [ $rc -eq 137 ] || [ $rc -eq 143 ]; then
    echo "$phase" > "$STATE"
    die "phase $phase exceeded ${PHASE_TIMEOUT}s and was killed. Inspect $plog"
  fi

  # --- billing tripwire -----------------------------------------------------
  # Documented (code.claude.com/docs/en/costs.md): the prompt-cache lifetime "is an
  # hour on a subscription and drops to five minutes once you're drawing on usage
  # credits; on an API key or cloud provider, it's five minutes by default." So a
  # 5-minute cache appearing here means this phase left the subscription pool and
  # is spending real money. Halt rather than discover it in the morning.
  # (Only valid while ENABLE_PROMPT_CACHING_1H is unset — it would mask the signal.)
  if [ "${ALLOW_USAGE_CREDITS:-0}" != "1" ]; then
    if [ "$parse_ok" != "1" ]; then
      # Fail CLOSED: an unreadable envelope is not evidence of safety.
      echo "$phase" > "$STATE"
      die "phase $phase produced no readable result envelope, so the billing
       tripwire could not be evaluated. Refusing to start another phase blind.
       Inspect $rlog and $plog, then resume with:
         dev/night_run/run_night.sh $phase"
    fi
    if [ "$c5m" -gt 0 ]; then
      echo "$phase" > "$STATE"
      die "phase $phase used a 5-MINUTE prompt cache ($c5m tokens), which means it was
       NOT drawing on the subscription — usage credits, an API key or a cloud
       provider was billed. Stopping before the next phase spends more.
       Check claude.ai/settings/usage, then resume with:
         dev/night_run/run_night.sh $phase
       Override deliberately with ALLOW_USAGE_CREDITS=1."
    fi
  fi

  # --- the real success test: did THIS phase commit? ----------------------
  # "A new commit appeared" is not enough. Measured on the 13 Aug run: the session
  # prompted for 19e found the tree red, spent itself finishing 19d, and committed
  # under 19d's header — the driver counted that as "19e done" and marched on for
  # two more phases. The commit must NAME the phase that just ran.
  if [ "$before" = "$after" ]; then
    echo "$phase" > "$STATE"
    die "phase $phase produced NO commit. Refusing to start the next phase on an
       unverified foundation. Inspect $plog and 'git status'."
  fi

  subjects=$(git log --format=%s "$before..$after")
  last_subject=$(git log -1 --format=%s "$after")
  log "Phase $phase committed: $(echo "$subjects" | tr '\n' '|')"

  if ! echo "$subjects" | grep -qiE "(^|[^0-9a-z])$phase([^0-9a-z]|\$)"; then
    echo "$phase" > "$STATE"
    die "phase $phase committed, but no commit names it:
         $(echo "$subjects" | head -3 | tr '\n' '|')
       The session did something else (typically: it finished the PREVIOUS phase
       instead). $phase itself has not run. Re-run it:
         dev/night_run/run_night.sh $phase"
  fi

  if echo "$last_subject" | grep -qi -- "partial"; then
    if [ "$attempt" -ge "$MAX_ATTEMPTS" ]; then
      echo "$phase" > "$STATE"
      die "phase $phase is still '— partial' after $attempt attempts. Stopping for
       a human. Its DONE summary in CLAUDE.md says what is left.
       Continue with: dev/night_run/run_night.sh $phase"
    fi
    log "Phase $phase committed PARTIAL — re-running it to finish (attempt $((attempt+1)))"
    echo "$phase" > "$STATE"
    continue          # same index: retry this phase in a fresh session
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
  pi=$((pi + 1))
done

log "ALL SELECTED PHASES COMPLETE"
log "commits this run:"
git log --format='  %h %ad %s' --date=short "$RUN_BASE..HEAD" 2>/dev/null \
  | tee -a "$LOGDIR/driver.log"
