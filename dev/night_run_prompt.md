# Night-run preparation prompt

**How to use this file.** Open a **fresh interactive Claude Code session** (VS Code extension is
fine), fill in the three placeholders below with tonight's specifics, and paste the whole thing —
from *"You are preparing an unattended overnight run"* to the end — as your first message.

The session will build the driver script and the per-phase prompt files, show you the exact bash
command, and stop. **It does not start the run.** You read the generated prompts, then launch it
yourself and go to bed.

---

## Fill these in for tonight

```text
<<< PLACEHOLDER 1 — PROMPTS COMMON PREFIX >>>

We are near the end of the development of tabxplor 2.0.0 and we want to simplify and integrate the
functions in tabxplor in a clear, simple and user-friendly ecosystem at the package level, to
simplify code, simplify future development, and make the whole framework more readable for both
human and machine. We are doing **"### Phase 19 — ecosystem integration round 2 roadmap"**, based on
`dev/tabxplor_phase19_ecosystem_integration.md`. Please plan for implementation then implement **"<Phase header>"**

<<< END PLACEHOLDER 1 >>>
```

```text
<<< PLACEHOLDER 2 — PHASES TO RUN TONIGHT >>>

"Phase 19d" to "Phase 19k"

<<< END PLACEHOLDER 2 >>>
```

```text
<<< PLACEHOLDER 3 — PROMPTS COMMON SUFFIX >>>

- **Internals and outputs are redesigned as radically as needed** for consistency, **integration of
  all subsystems into a consistent ecosystem**, and reaping of the simplification rewards of the new
  framework.
- **No back-compatibility needed at all on regression functions and jamovi UI**: user API too can be
  radically changed for user-friendliness. **For tabxplor 1.3.1 public API, we can often route old
  arguments to new ones when needed**, and do ad hoc back-compat *after* having found a better
  framework and API.
- Do NOT add another layer of confusion and ad-hoc solutions inside the code: your main aim is to
  simplify, to remove traces of old implementations altogether when they have become useless, to
  clarify, to integrate the new features in the current code seamlessly, and to increase
  user-friendliness in any situation.

<<< END PLACEHOLDER 3 >>>
```

---

## The prompt to paste (everything below this line)

You are preparing an **unattended overnight run** of the tabxplor roadmap. The maintainer will
launch it and go to sleep; nobody will answer anything until morning. Your job in *this* session is
**only to build the machinery and then stop** — you must not implement any roadmap phase yourself,
and you must not start the run.

Produce four things:

1. `dev/night_run/rules.md` — the standard instruction block, passed to every phase session as a
   **system prompt**.
2. `dev/night_run/prompts/<phase>.txt` — one user prompt built per phase to run tonight.
3. `dev/night_run/run_night.sh` — the driver.
4. The exact bash command to launch it, printed in your final message.

### The three prompt parts

Each per-phase prompt file is assembled as **PREFIX + the phase name + SUFFIX**, plus a short reading
list you add (see below). Take the prefix and suffix verbatim from PLACEHOLDER 1 and PLACEHOLDER 3
above — do not paraphrase them, do not "improve" them.

The **third common part is the standard night-run block, and it does NOT go in the user prompt.** It
goes in `dev/night_run/rules.md`, passed with `--append-system-prompt-file`. That placement is
load-bearing and verified: the system prompt is the one region auto-compaction never touches, so
these rules survive a long phase verbatim, while a prompt-borne copy would be summarised away — which
is exactly how an unattended run drifts into asking a question at 03:00 and stalling until morning.

Write `rules.md` from the block below. Reproduce its substance faithfully; you may reorganise for
clarity and you **should** add anything else you judge relevant from `CLAUDE.md` (its § Testing
recipe, the orphaned-worker warning, the sandbox exceptions, the Suggests guards, the golden
discipline, the "last step of every implementation" documentation duties). Do not soften it.

---

#### THE STANDARD NIGHT-RUN BLOCK

**Working unattended**

- **Never use `AskUserQuestion`**, do everything **without pauses or questions asked**: the
  maintainer is absent until tomorrow. If you see some caveats, inconsistencies, decisions not yet
  settled, or white elephants in some of my propositions, please tell me honestly in the DONE
  summary. For the questions that arise, decide yourself based on the choices already made; when you
  see a more integrated, reliable, consistent, user-friendly, modern, future-proof way to do the same
  thing, consistent with the initial plan, do it yourself without questions asked.
- **Never enter plan mode and never call `ExitPlanMode`.** Write the plan as ordinary text, then
  implement it in the same session. There is nobody awake to approve a plan.
- **Never stop to check in.** No "shall I proceed?", no "let me know which you prefer", no ending the
  turn early to report progress. The phase is finished when its exit criteria are met, or when you
  hit a blocker you genuinely cannot work around.
- **Never guess what something is.** Do not answer a factual question about the codebase, a tool, or
  a CLI from memory when you can probe it. If a fact is not stored, storing it is the task.

**Scope of a phase**

- Implement the **whole** phase, not the easy part. If part of it is genuinely blocked, finish
  everything else in full and say precisely what you left out and why.
- **Delete the superseded implementation in the same phase.** No commented-out corpses, no "kept just
  in case" branch, no fifth variant added beside four old ones.
- Do not invent scope beyond the phase entry. A better idea that belongs to a later phase goes in the
  DONE summary as a follow-up, not into tonight's diff.

**Verification**

- Use the `CLAUDE.md` § Testing recipe exactly: a temp `.R` file **outside `tests/`**, with
  `Sys.setenv(TESTTHAT_CPUS = "8", NOT_CRAN = "true")`, run as `OMP_NUM_THREADS=1 Rscript <file>.R`.
- Run test suites in the **foreground** with a long timeout. **Never kill a run by killing its
  parent** — it orphans workers that then burn CPU for hours and silently starve every later phase.
  Never run two R test processes at once.
- `devtools::document()` and `test-parallel-parity.R` must run **unsandboxed**.
- Never run `devtools::check()` and never run the CI-locale pass. Those belong to the final
  documentation/release phase alone.
- **Goldens**: prove the phase's declared delta with `dev/verify_golden_field_delta.R`. A phase
  declared byte-identical that shows golden churn is a **failure to investigate**, never a snapshot
  to accept.
- A claimed fix ships with the fixture that fails without it.

**Documentation duties, before the commit**

- File-header docstrings of every modified module still accurate; `# DESIGN:` / `# WARNING:` tags
  updated next to changed logic.
- `dev/tabxplor_architecture.md` updated where the structure really moved — no clutter, no
  restating of details.
- A `#### Phase <x> — <title>` DONE summary appended to `CLAUDE.md` in the roadmap section.
- `NEWS.md` only if something is genuinely user-facing or CRAN-facing.

**DONE summary** in `CLAUDE.md`, including **HONEST CONCERNS** and **FOLLOW-UPS** when needed.

**Finish the session with a commit**
- **One commit per phase, made at the very end, once everything is finished** — implementation,
  tests, goldens and documentation all done and the working tree otherwise clean. Never commit
  work-in-progress partway through.
- **The commit message is the exact phase header, and nothing else.** For example:
  `Phase 19d — KEY 8a: the tab() comparison surface`. No body, no summary, no bullet list of changes,
  no file list.
- **Never add a `Co-Authored-By` trailer and never add a "Generated with Claude Code" line.** The
  maintainer authors and signs every commit.
- Commit on the `dev` branch. Do not push, do not open a PR, never touch `master` or `release/*`.
- Never `git reset --hard`, `git checkout -- .`, `git clean`, or amend a commit you did not make in
  this same phase. Earlier commits are inviolable.
- The commit is the driver's success signal: **a phase that does not commit halts the whole night.**
- The commit should be the last thing done in the session : it triggers the next Phase in a fresh headless session.

**If you are blocked**

Complete every independent part of the phase in full, commit what genuinely works with the phase
header suffixed ` — partial`, write `PHASE <x>: BLOCKED` and say exactly what stopped you, then stop.
The driver will halt the night rather than start the next phase on a foundation that is not there.

---




### Building the per-phase prompt files

For each phase chosen in PLACEHOLDER 2, build `dev/night_run/prompts/<phase>.txt` (do not write it yourself, create a script to build it from the phases list, prefixes and suffixes) containing:

1. The PLACEHOLDER 1 **PREFIX** verbatim, with the phase name appended so the sentence completes naturally.
2. The PLACEHOLDER 3 **SUFFIX** verbatim.

"THE STANDARD NIGHT-RUN BLOCK" above is written in the system prompt, not the user prompt.

Flag to the maintainer in your final message — do not silently drop them — any phase in tonight's
range that is a poor fit for unattended work. In particular a phase whose own entry says it is
creative, open-ended, or should be discussed first, and any large single-shot structural migration
whose rule is "never leave a representation half-migrated" (a watchdog kill mid-migration leaves
exactly the forbidden state). Recommend running those awake.

### Building the driver

`dev/night_run/run_night.sh`, bash, `set -uo pipefail`, executable. Requirements — these come from
forensics on this machine's real past night runs, so implement them all:

- **One fresh `claude -p` per phase.** No `--continue`, no `--resume`, no reused `--session-id`. A new
  process is the only way to get a genuinely clean context. The model **cannot** invoke `/compact`
  itself — it is user-typed only — so "compact between phases" is not an option that exists.
- Pass, on **every** invocation: `--append-system-prompt-file dev/night_run/rules.md`,
  `--permission-mode bypassPermissions` (it is never restored across invocations),
  `--model opus`, `--effort high`, `--output-format stream-json --verbose`, and
  `--max-budget-usd <cap>` as a runaway guard (**`--max-turns` no longer exists** in Claude Code
  2.1.220 — do not use it).
- **Preflight**: abort unless the current branch is `dev` and `git status --porcelain` is empty. The
  success signal is meaningless on a dirty tree.
- **Success = a new commit appeared**, checked with `git rev-parse HEAD` before and after. Never the
  model's own claim. No commit → halt the run.
- **Stall watchdog**: kill the phase if its event-stream log stops growing for ~30 minutes. This is
  the most valuable part of the driver. Measured on the runs it replaces: one Bash tool call blocked
  for **7 h 45 m**, and one session died silently at 01:10 and sat dead until the maintainer woke at
  04:21 — **5 h 12 m lost with nothing watching.**
- **Per-phase wall-clock timeout** (~4 h) as a second backstop.
- **Detect usage / spend limits** in the result envelope and stderr (a past run had ~8 subagents
  killed by a monthly spend limit, then a 429). On a limit: save state and halt cleanly, so the next
  night resumes at the right phase rather than redoing or skipping one.
- **Resume state** in a git-ignored file, plus optional `start`/`end` phase arguments and a `DRY_RUN=1`
  mode that writes the prompts and prints the plan without calling `claude`.
- **Logs** under a timestamped `dev/night_run/logs/<stamp>/` — per-phase `stream-json`, stderr, the
  final result envelope, and a `driver.log` with one line per phase (exit code, cost, session id,
  resulting commit).
- Git-ignore `logs/` and the state file. `dev/` is already `.Rbuildignore`'d.

Cost note for sizing the budget cap: a cold `claude -p` in this repo costs about **$0.68 and ~162k
tokens** before any work, because `CLAUDE.md` alone is ~39k tokens.

If `dev/night_run/` already contains a driver or rules from a previous night, **read it first and
update it in place** rather than adding a parallel second one.

### Finally

- **Verify the driver without running a phase**: `bash -n` it, and exercise `DRY_RUN=1`.
- Do **not** launch the night run.
- End with: the list of prompt files you wrote, any phase you flagged as unsuitable for unattended
  work, and the exact bash command to start the run — including how to resume it tomorrow if a limit
  is hit, and where the logs will be.
