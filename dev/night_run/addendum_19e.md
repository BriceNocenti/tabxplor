## Addendum — read this before planning

Two phases landed **out of the roadmap's order** while you were not run, so the repository you are
about to plan against is not the one the plan-of-plans describes. Verify each claim below in the
code before relying on it; they are stated to stop you planning against a clean slate, not to be
trusted blindly.

### 1. Nothing of 19e exists yet — the whole scope is yours

The session that held 19e's slot never started it. It found the tree red from 19d's partial commit,
which its own summary warned against building on, and spent itself driving **FAIL 48 → 8**. Its
report says plainly: *"Nothing of 19e's own content was implemented."* Confirmed at this commit —
`exponentiate` (28 uses), `estimate_display` (31), `ame_ratio` (47) are all still live, and there is
no `measure` argument anywhere.

So: the full 19e entry, unchanged in scope.

### 2. 19g already landed, and the roadmap says it should have come after you

Its dependency line reads *"19g must land after 19e, 19f"*. It did not. 19g is committed
(`c3c3c25`), the suite is at **FAIL 8 / PASS 6001**, and the 8 are the pre-existing
`test-jmvtab-cache.R` failures — not 19g's doing.

The practical consequence is that **19e must fit itself to structures that already exist** rather
than introduce them. What 19g built, all of it live:

- **`meta$spec`** — `R/table-spec.R`, `new_spec(kind, vars, call)`. `kind` is *stated* by the
  producer and read through `tab_kind()` / `tab_is_reg()`; `is_reg_footer()` is deleted. The old
  `meta$reg_meta` is gone: **`spec$call` is the producer's recipe now**, and it is what
  `reg_check_plots()` refits from (`fit_spec`). Your new estimand arguments are part of that recipe —
  a table must remember the `effect` × `measure` it was built with, or a refit silently changes the
  estimand.
- **`new_reg_shared()`** — the `shared` bag is a typed record whose **formals are the contract**, and
  `fmt_class.R`'s `globalVariables()` mirror is derived from them. Add `effect`/`measure` to the
  constructor; do not thread a loose argument past it.
- **One map, per-spec builders.** The three column builders (AME / MNL-vs-rest / coefficient) sit
  behind a single map with a **per-spec** choice, replacing a table-scalar `if`. `family` has been
  per-dependent since 15e. **`measure` and `effect` must be per-spec on the same footing** —
  scalar / vector / named vector, resolved exactly where `family_for` is. A table-scalar estimand
  would re-introduce the degradation 19g just removed.
- **`REG_GOF_KEYS` + `reg_stat_keys()` + `reg_validate_stat_keys()`** — one vocabulary, one
  validator, for `stats =` and `check =`. If your capability table or `measure` vocabulary needs the
  same treatment, copy this shape; do not add a second hand-written list.
- **The `test` tibble is re-keyed**: `var` (which variable the row is about, `term` folded into it),
  `col`, and the sub-population in a column named after the grouping variable. 13 columns, not 14.

**Report, in your DONE summary, whether 19g needs a corrective pass** now that the estimand is named:
specifically whether `spec$call` records enough to reproduce the estimand, and whether
`spec$vars` — built before `measure` existed — is still complete. Do not silently patch 19g's work
into your own diff without saying so.

### 3. 19b's stored scale is in — use it, do not re-derive

KEY 2 landed: `get_scale()` / `get_pct_base()` are live and `EST_SCALES` / `est_scale_key()` are the
stored library. 19e's own text depends on this ("once `measure` names the estimand and KEY 2 stores
its scale, a reg table's `color` needs only `TRUE`/`FALSE` + `adjustment`/`between_groups`"). Read
the scale off the column; never sniff it back from `var`, a label, or a display string.

### 4. Out of scope — do not be drawn in

- **The 8 `test-jmvtab-cache.R` failures.** Pre-existing, quarantined, unchanged through 19f and 19g,
  and explicitly assigned to **19l** as a mechanical pass. Leave them red. If your work makes them
  worse, that is yours to fix; closing them is not.
- **The ~124 deprecation WARNINGs** from the test corpus still calling `ci = "diff"` etc. A known
  19d follow-up, not 19e's.

### 5. Sequence the work so a partial commit is still coherent

Two of the four sessions on the 13 Aug run were cut off before finishing. The constraint is the
**5-hour session window**, not money. Order 19e so the highest value lands first and any forced stop
leaves a consistent state rather than a half-migrated one:

1. **`family = "rr"` through the front door.** The plan calls this "the change with the largest ratio
   of user value to work in the whole phase" — today the only route to a risk ratio is naming the
   wrong distribution, and asking for it directly is refused.
2. `effect` x `measure` with the synonym table, and the deletion of `exponentiate` / `at` /
   `ame_ratio`.
3. `estimate_display` -> a real `display =`.
4. The three-state capability table as a runtime object with its four consumers.

If the window runs short, stop at a boundary between these, commit with `- partial`, and say in the
summary exactly which of the four landed and what state the rest is in - the way 19d did. The driver
re-runs a partial phase in a fresh session to finish it, so a truthful partial is a checkpoint. A
phase cut off mid-migration is not.
