# Unattended night runs

Three files, two commands. No preparation session, no AI in the loop before the run starts.

| file | role |
|---|---|
| `rules.md` | the standing rules, passed to **every** phase as a **system prompt** |
| `build_prompts.sh` | turns a prefix + a phase range + a suffix into `prompts/<phase>.txt` |
| `run_night.sh` | the driver: one fresh `claude -p` per phase, git commit as the checkpoint |

`prompts/`, `logs/` and `.state` are derived and git-ignored.

## Run one

```bash
cd ~/github/tabxplor

read -r -d '' PREFIX <<'EOF'
We are near the end of the development of tabxplor 2.0.0 and we want to simplify and integrate the functions in tabxplor in a clear, simple and user-friendly ecosystem at the package level, to simplify code, simplify future development, and make the whole framework more readable for both human and machine. We are doing **"### Phase 19 — ecosystem integration round 2 roadmap"**, based on `dev/tabxplor_phase19_ecosystem_integration.md`. Please plan for implementation then implement
EOF

read -r -d '' SUFFIX <<'EOF'
- **Internals and outputs are redesigned as radically as needed** for consistency, **integration of all subsystems into a consistent ecosystem**, and reaping of the simplification rewards of the new framework.
- **No back-compatibility needed at all on regression functions and jamovi UI** : user API too can be radically changed for user-friendliness. **For tabxplor 1.3.1 public API, we can often route old arguments to new ones when needed**, and do ad hoc back-compat *after* having found a better framework and API.
- Do NOT add another layer of confusion and ad-hoc solutions inside the code: your main aim is to simplify, to remove traces of old implementations altogether when they have become useless, to clarify, to integrate the new features in the current code seamlessly, and to increase user-friendliness in any situation.
EOF

# 1. build the prompts
dev/night_run/build_prompts.sh --from 19d --to 19k --prefix "$PREFIX" --suffix "$SUFFIX"

# 2. read a couple of them, then launch
cat dev/night_run/prompts/19d.txt
dev/night_run/run_night.sh
```

`read -r -d ''` returns 1 at EOF, so don't put it under `set -e`. The quoted `<<'EOF'` keeps
backticks and `$` literal — the prefix contains both.

## build_prompts.sh

Phase ids, order and **exact titles** are read from the roadmap, never typed. That matters: the
phase is told to use "the exact phase header, and nothing else" as its commit message, and the
driver reads that commit back as the success signal.

```bash
dev/night_run/build_prompts.sh --list                      # what the roadmap declares
  --from 19d [--to 19k]        # a range in roadmap order; --to defaults to the last
  --phases "19d 19e 19g"       # or an explicit list
  --prefix / --suffix          # verbatim text
  --prefix-file / --suffix-file
  --roadmap <f>                # default dev/tabxplor_phase19_ecosystem_integration.md
```

It rewrites `prompts/` from scratch each time and writes a `PHASES` manifest the driver reads, then
flags any selected phase the docs themselves call creative, to-be-discussed-first, or
not-to-be-interrupted. Consider running those awake.

## run_night.sh

```bash
dev/night_run/run_night.sh              # every phase in the manifest, or resume from .state
dev/night_run/run_night.sh 19f          # start here
dev/night_run/run_night.sh 19f 19h      # inclusive range
DRY_RUN=1 dev/night_run/run_night.sh    # write prompts to the log dir, call nothing

MAX_BUDGET_USD=25 PHASE_TIMEOUT=14400 STALL_TIMEOUT=1800 dev/night_run/run_night.sh
```

**Preflight**: aborts unless you are on `dev` with a clean tree. The success signal is "a new commit
appeared", which is meaningless otherwise.

**Per phase**: a fresh `claude -p` — no `--continue`, no `--resume`. A new process is the only way
to get a genuinely clean context; the model cannot invoke `/compact` itself, so "compact between
phases" is not an option that exists. `--permission-mode bypassPermissions` is re-passed every time
(it is never restored), and `rules.md` rides in via `--append-system-prompt-file`, where
auto-compaction cannot reach it. `CLAUDE.md` loads itself from the working directory.

**Halts the whole night** on: no commit, a stall (30 min of silence in the event stream), the
wall-clock timeout, a usage/spend limit, or the **billing tripwire**. It writes `.state` first, so
the next night resumes at the right phase.

### The billing tripwire

`costs.md` states the prompt-cache lifetime "is an hour on a subscription and drops to five minutes
once you're drawing on usage credits; on an API key or cloud provider, it's five minutes by
default." The result envelope reports both counters, so after each phase the driver reads
`usage.cache_creation` and **halts if any 5-minute cache appears** — that is the run leaving the
subscription pool and spending real money.

`parse_result.py` **fails closed**: an unreadable envelope halts too, because "0 tokens" and "could
not parse" must never look alike. Override deliberately with `ALLOW_USAGE_CREDITS=1`.

Only valid while `ENABLE_PROMPT_CACHING_1H` is unset — setting it restores the 1-hour lifetime on
credits and would mask the signal. Do not set it.

This is a detector, not a guarantee. The hard guarantee is turning usage credits off at
[claude.ai/settings/usage](https://claude.ai/settings/usage).

**Logs** in `logs/<stamp>/`: per-phase `stream-json`, stderr, the result envelope, and `driver.log`
with one line per phase (exit code, cost, session id, resulting commit).

## Why it is shaped like this

From forensics on the hand-driven runs it replaces:

- Every past "night run" was **one continuous session**, never restarted, and **never compacted** —
  peak context 520–728k of the 1M window. Context was never the failure.
- A Bash tool call once blocked for **7 h 45 m**.
- One session died silently at 01:10 and sat dead until the maintainer woke at 04:21 — **5 h 12 m
  lost with nothing watching**. Hence the stall watchdog.
- ~8 subagents were once killed by a monthly spend limit, then a 429. Hence limit detection and
  `--max-budget-usd`.
- The old prompt template ended by inviting `AskUserQuestion` — a built-in stall. Hence `rules.md`.

A cold `claude -p` here costs about **$0.68 / 162k tokens** before any work (`CLAUDE.md` alone is
~39k), which is the per-phase floor.
