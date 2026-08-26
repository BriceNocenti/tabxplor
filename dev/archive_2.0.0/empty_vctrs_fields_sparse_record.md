# Phase 18z6 — should `tabxplor_fmt` stop creating its all-`NA` fields?

**Status**: study + measurements, no code changed. Decisions for the maintainer are in §9.
**Date**: 2026-08-05. **Baseline**: `dev` @ z5 (20 fields, 11 column attributes).

---

## 0. Executive summary

The question was: *could `obs` (and the other always-`NA` fields) simply not be created unless
something writes them — and would that buy anything?*

Measured answers, in order of how much they should weigh:

| Question | Answer |
|---|---|
| Is it **technically possible** with vctrs? | **Yes.** Records with different field sets combine correctly through tabxplor's own `vec_ptype2`/`vec_cast` (measured, §2). |
| Would it **speed up** anything? | **No.** A whole `tab()` build makes **21 `new_fmt()` calls**. Field count costs ~0.7 µs/field/call. Best case saving: **~0.2 ms out of 650 ms = 0.03 %** (§4). |
| Would it **save memory**? | Marginally. All-`NA`/all-`FALSE` fields are **42 % of an fmt column's field bytes = 30 % of the whole table object** — but a big realistic `tab_many()` (86 rows × 17 fmt columns, 1462 cells) is **308 KB total**, so the ceiling is **~92 KB** (§5). |
| Would it **simplify** the code? | **No — it does the opposite.** It replaces one fixed, snapshot-locked 20-field shape with a per-column variable shape, adds a guard to 16 getters + 16 setters + `vec_ptype2` + 3 casts + 2 arith + `$` + `mutate()`, and makes `test-fmt-contract.R`'s central guarantee unstateable (§3, §6). |
| Can `get_*` / `$` return `NA` without creating the field? | **Yes**, one guard in `fmt_field_factory()` and one in `` `$.tabxplor_fmt` ``. |
| Can `set_*` / `$<-` create it reliably? | **Not with `` vctrs::`field<-` ``** — it *errors* on an absent field (measured, §2.3). Every setter would need a rebuild path. |
| Does `mutate()` still work? | **Not without a shim.** The data mask cannot see an absent column; `mutate(col, pct = pct + diff)` on a column lacking `diff` errors. This is explicitly user-contract surface (CLAUDE.md § Deprecation) (§3.4). |

**Recommendation: do not make the record sparse.** Instead take the *one* thing the idea was really
pointing at — that `new_fmt()` allocates 17 separate `NA` vectors for fields nobody set — and fix
that inside the constructor. A prototype is **4.2× faster per call, byte-identical output, zero API
change** (§7). It is still invisible end-to-end, so it is a hygiene fix, not a perf phase.

---

## 1. What "empty" actually means today

`vec_data()` on real tables, counting a field as *empty* when it is all-`NA` (doubles) or all-`FALSE`
(the three `in_*` flags):

| table | empty fields (of 20) | which |
|---|---|---|
| crosstab, `pct="row"`, `ci="cell"` | **10** | `wn, mean, ctr, var, pvalue, or, n_eff, obs, in_tottab, in_refrow` |
| table of means | **12** | `wn, pct, ctr, ci_inf, ci_sup, pvalue, or, tot_n, n_eff, obs, in_tottab, in_refrow` |
| `color = "contrib"` | **8–9** | `mean, ci_inf, ci_sup, or, n_eff, obs, in_tottab, in_refrow` |
| weighted, 60×20×4, `wt=` | **8** | `mean, ctr, var, pvalue, or, n_eff, obs, in_refrow` |
| big `tab_many()` (6 row_vars × 2 col_vars) | **8** on every column | — |
| `tab_reg(empirical=)` | ~10 | the crosstab-only fields |

Emptiness measured per FIELD across every fmt column of three representative tables:

```
or, n_eff, obs, in_tottab, in_refrow   100 % of columns empty
mean                                    87 %
pvalue                                  62 %
wn, ctr, ci_inf, ci_sup                 56 %
var                                     44 %
pct, tot_n                              12 %
n, display, digits, diff, ratio, in_totrow   0 %  (always populated)
```

Two things to read off this:

1. There is no field that is *always* empty in every use — `or`, `n_eff` and `obs` are 100 % empty
   only because these three tables did not ask for OR colouring, Kish `n_eff` or `tab_reg`. The
   field set is already **minimal in the no-redundancy sense** (the c-iii audit conclusion still
   holds: nothing is vestigial, nothing is derivable from another field — `ci` and `resid` are the
   two quantities that already *are* derived and correctly have no field).
2. **`NA` is the honest encoding of "this measure does not apply here"**, and the colour engine
   depends on it: `fmt_adjustment_score()`, `get_num()`'s `obs` arm and the tooltip builder
   (`tab_classes.R:2238`) all read `get_obs(x)` unconditionally on *every* column, crosstabs
   included, and leave the cell uncoloured because the value is `NA`. That is z5's stated design.

---

## 2. Feasibility — what vctrs allows and what it refuses

Probed on a minimal record with an optional `extra` field plus union-`vec_ptype2` and filling-`vec_cast`
(the exact shape tabxplor would need).

### 2.1 What works

`vec_c(a, b)`, `vec_c(b, a)`, `c(a, b)`, `vec_rbind()`, `dplyr::bind_rows()`, `vec_slice()`,
`vec_init()`, `vec_assign()`, `vec_proxy_equal()` and tibble printing **all work** across records
with different field sets, provided ptype2 unions the fields and cast fills the missing ones with
`NA`. vctrs does not require a fixed field set; the record's proxy is just its data frame.

So the *combination* semantics are not the obstacle.

### 2.2 What needs a guard

`vctrs::field(x, "obs")` on an absent field **errors**:

```
Error: Invalid index: field name 'obs' not found
```

so all 16 `fmt_field_factory()` getters, the 23 direct `vctrs::field()` reads in `R/`, and
`` `$.tabxplor_fmt` `` need a presence check. That part is mechanical and cheap.

### 2.3 What vctrs refuses outright

`` vctrs::`field<-`(x, "obs", value) `` on an absent field **errors with the same message — it cannot
create a field.** Every one of the 16 `fmt_set_field_factory()` setters and the 9 direct
`` `field<-` `` writes would need a second code path that rebuilds the whole record
(`new_rcrd(c(vec_data(x), list(obs = value)), !!!attrs)`) in canonical field order. So a "cheap
setter" does not exist: writing a field for the first time becomes a full-column reallocation of all
20 fields, in the middle of the build.

---

## 3. The change surface, counted

| Site | Count | Change needed |
|---|---|---|
| `fmt_field_factory()` getters | 16 | presence guard returning the right typed `NA` vector |
| `fmt_set_field_factory()` setters | 16 | rebuild-if-absent path |
| direct `vctrs::field()` reads in `R/` | 23 | audit each |
| direct `` `field<-` `` writes | 9 | audit each |
| `new_fmt()` call sites | 29 | pass `NULL` instead of relying on the `NA` default |
| public `fmt()` call sites | 19 | same |
| `vec_ptype2.tabxplor_fmt.tabxplor_fmt` | 1 | union the field sets (today it rebuilds a full 20-field ptype) |
| `vec_cast` methods | 3 | fill absent fields to the target's set |
| `vec_arith` methods | 6 | omit rather than `NA`-fill |
| `vec_math` sum/mean arms | 2 | same |
| `$` / `mutate()` | 2 | §3.4 |
| jamovi carrier (`as.list(vec_data(col))` → `do.call(new_fmt, …)`) | 2 | works unchanged, but **schema bump** (stored shape changes) |
| `test-fmt-contract.R` + its snapshot | 1 file | §6 |
| field readers across the package (`get_*`/`is_*`) | 265 call sites | rely on the guarded getters — no edit, but a check now runs where none did |

This is the `/vctrs-field` checklist run **twenty times over**, plus a new invariant that did not
exist before ("which fields does this column have?").

### 3.4 The user-contract problem

CLAUDE.md § Deprecation is explicit: *"Some user code rely on `tabxplor_fmt` vctrs fields extracted
with `$` or calculated with `mutate()` … the vctrs fields should not break."*

* `$` — fine. `` `$.tabxplor_fmt` `` already special-cases `wn`, `ci` and `tot_wn`; adding "absent →
  typed `NA` vector" is one more branch.
* `mutate()` — **not fine.** `mutate.tabxplor_fmt()` mutates the `vec_proxy()` data frame; a data
  mask cannot resolve a column that is not there, so `mutate(col, pct = pct + diff)` on a column
  without `diff` errors with `object 'diff' not found` where it silently returns `NA` today. The fix
  is to materialise the full 20-field frame inside `mutate.tabxplor_fmt()` and re-sparsify after —
  i.e. **the one entry point where users touch fields would go back to being dense**, which rather
  undercuts the exercise.

---

## 4. Performance — measured, and it is not there

Instrumented `new_fmt()` over real builds:

| build | `new_fmt()` calls | cells constructed |
|---|---|---|
| `tab()` 60 × 20 × 4 subtables, weighted, `ci="cell"` | **21** | 5145 |
| `tab()` with a numeric col_var | **22** | 1342 |
| `tab_reg(empirical = TRUE)` | **3** | 186 |

Per-call cost (60-cell column, microseconds):

```
bare new_rcrd,  8 fields                27.0
bare new_rcrd, 20 fields                36.7      -> ~0.7 us per extra field
new_fmt(), all 17 doubles supplied      49.1
new_fmt(), display supplied             69.3
new_fmt(), everything defaulted        168–254    <- the DEFAULTS are the cost, not the fields
   of which: 17x rep(NA_real_, 60)       15.9
   of which: dplyr::case_when(display)   90.2     <- one case_when inside a constructor
```

End-to-end: the big `tab_many()` above builds in **624 ms** and prints in **115 ms**. Twenty-one
constructor calls saving ~10 µs each is **0.2 ms = 0.03 %**, three orders of magnitude below run-to-run
noise. Even the far larger saving from §7's prototype (≈190 µs × 21 = 4 ms) **did not show up
end-to-end** (720 ms vs 624 ms across runs — noise, in the wrong direction).

The build time lives in the data.table aggregation and the dplyr assembly, exactly where the
`tab_many` performance profile already put it. **Field count is not on the critical path.**

Against that, sparse fields would *add* cost in three places: a presence check inside every one of
265 getter call sites per render; a set-union in `vec_ptype2` (which the compact merge calls per
column, the hottest fmt path there is); and a full-column rebuild the first time any field is
written mid-build.

---

## 5. Memory — real, small, and bounded

Per-cell, a 20-field fmt column costs ~143 bytes of field data (+ attributes). For the biggest
realistic table measured:

```
tab_many, 6 row_vars x 2 col_vars: 86 rows x 17 fmt columns = 1462 cells
  whole object                308 KB
  field bytes                 217 KB
  all-NA / all-FALSE fields    92 KB   (42 % of field bytes, 30 % of the object)
```

So the **absolute ceiling for a sparse record is ~92 KB on a large table**, ~40 KB on an ordinary
one. Twenty full tables held at once measured **4.7 MB**. fmt memory scales with **cells, not rows**,
so the 8M-row benchmark fixtures do not change this — their tables have the same few thousand cells.

One measurement worth recording because it reframes the whole question. Allocating 17 `NA` fields of
10⁶ cells:

```
17 separate rep(NA_real_, n)            129.7 MB
ONE rep(NA_real_, n) shared 17 times      9.9 MB     (13x less; copy-on-write keeps it correct)
```

`new_fmt()` today evaluates `rep(NA_real_, length(n))` **17 separate times** (confirmed: 20 distinct
SEXP addresses in a fresh `new_fmt(n = 1:5)`). **Most of the "wasted" memory is not the fields'
existence — it is that they are seventeen distinct allocations of the same constant vector.** That is
fixable without touching the record shape (§7).

---

## 6. The architectural argument (the one that actually decides it)

Phase 17's rule 1 is *simplify and integrate, never add another ad hoc layer*, and rule 2 is *roles
are stored, never guessed*. A sparse record works against both:

* **It turns a fixed shape into a variable one.** Today every fmt column has exactly 20 fields;
  `test-fmt-contract.R` states that as *the* contract and is deliberately brittle so a shape change
  fails loudly. With optional fields, `fields(x)` becomes per-column data and the contract test can
  no longer say what the record *is* — it could only enumerate what fields are *allowed*, which is a
  strictly weaker guarantee for a type users read with `$`.
* **It replaces a value-level question with a structural one.** "Does this cell have an observed
  effect to compare against?" is answered today by `is.na(get_obs(x))` — one uniform idiom the colour
  engine, the tooltip and `get_num()` already share. Sparse fields add a *second*, structural way to
  ask the same thing (`"obs" %in% fields(x)`), and the two can disagree (a column where `obs` exists
  but is all `NA`). Two encodings of one fact, kept in sync by convention, is the §2.5 disease this
  roadmap has spent Phase 17 removing.
* **It buys nothing a user or a reader can see.** With the getter guards in place, `col$obs`,
  `get_obs(col)` and the rendered table are identical either way. The only observable difference is
  `vctrs::vec_data(col)` showing 12 columns instead of 20 — and if *that* is the real motivation, a
  three-line `fmt_fields_used(x)` introspection helper gives it with no risk at all.

The honest summary is that the 20-field record is not a white elephant. It is a **fixed-width row
type** — the same trade every columnar store makes — and its `NA`s are meaning, not waste.

---

## 7. What does pay off: fix the constructor, not the record

The measurements point at a different, much smaller target inside `new_fmt()`:

1. it evaluates `rep(NA_real_, length(n))` 17 times instead of once (13× the memory, §5);
2. its `display` default runs a `dplyr::case_when()` on three scalar conditions — **90 µs, more than
   half the whole constructor's cost** — where a two-branch `if` does the same thing.

**IMPLEMENTED** (maintainer decision 1 + 2 below): `new_fmt()` now takes `NULL` field defaults and
fills them in the body from one shared `nas` / `fls` vector, with a base-R `display` default
(`%in%`, not `==`, so an `NA` `type` falls through to `"n"` exactly as `case_when` did).

```
new_fmt(n = 60 cells, all defaults)   203.5 -> 106.6 us
new_fmt(type = "row") size-0 ptype    189.4 ->  62.3 us   <- the hot path (compact merge)
13 constructor shapes, identical(vec_data()) AND identical(attributes())   TRUE
distinct SEXPs in a fresh 20-field record:  20 -> 5
```

The size-0 ptype case is the one that matters: `vec_ptype2` calls `new_fmt()` with no field at all,
so it used to pay the full `case_when` (a size-independent 90 µs) on every column of every merge.
The sharing also partly survives the pipeline — in the **final** table column the 10 empty fields
occupy **5 distinct SEXPs instead of 10**, the rest duplicated by slicing/casting on the way.

`fmt()` (the public constructor) was **deliberately left alone**: it is called **0 times** on the
crosstab path and 3 times by `tab_reg`, its defaults are part of its documented usage, and it passes
every field explicitly to `new_fmt()` anyway.

Properties: byte-identical output, no signature change (the `NULL` defaults are internal — every
existing `new_fmt(...)` call site keeps working), no contract change, no snapshot churn, no jamovi
schema bump. Full suite after the change: **FAIL 0, WARN 0, SKIP 4, PASS 4479** — the z5 count exactly.

**Measured end-to-end effect: none, as predicted.** A same-session A/B over the big `tab_many()`
build (3 alternating pairs, 5 builds each) gave a median **691 ms new vs 679 ms old — −1.8 %, i.e.
noise, in the wrong direction**. 210 calls × ~100 µs ≈ 21 ms sits below this box's ±15 % run-to-run
variance. This is hygiene (one allocation instead of 17, no `dplyr::case_when()` inside a
constructor), **not** a performance change, and it is deliberately absent from NEWS.

---

## 8. Rejected alternatives

* **Two tiers ("always keep the base fields, make the rest optional").** The maintainer's own hedge,
  and it is the sane subset if we did this at all — but it makes the picture *worse* to reason about:
  a reader must now learn which fields are guaranteed and which are conditional, i.e. two concepts
  where there was one. It also does not shrink the change surface (§3) at all, since the guards are
  needed on exactly the fields that vary.
* **ALTREP "constant NA" vectors.** Would give real sparsity with no API change, but there is no
  base-R way to build one from R code; it needs C and an ALTREP class registration. Far too much
  machinery for ≤92 KB, and it would put a C dependency into a package that has none.
* **Dropping a field by deriving it.** Checked: no remaining field is a function of the others.
  `ci` (from `ci_inf`/`ci_sup`) and z4's `resid` (from `pvalue` + `sign(ctr)`) are the two quantities
  already handled that way, and correctly have no field. `n_eff`, `obs`, `ratio`, `tot_n`, `wn` are
  all independent inputs.
* **Storing `obs` as a column attribute instead of a field.** Fails immediately: `obs` varies per
  cell (one crude effect per predictor level), which is the definition of a field.

---

## 9. Decisions for the maintainer

1. **Sparse / optional fmt fields — implement, or close?**
   Recommendation: **close**, with §6 recorded as the reason (fixed record shape is a feature; the
   contract test depends on it; measured gain 0.03 % time / ≤92 KB).
2. **Constructor cleanup (§7) — do it now, or leave `new_fmt()` alone?**
   Recommendation: **do it** in a small byte-identical commit (shared `NA` vector + base-R `display`
   default). It is the honest residue of this phase. Verification: full suite green, zero golden
   churn — the prototype is already `identical()` on data and attributes.
3. **`fmt_fields_used(x)` introspection helper — wanted?**
   A one-liner returning the populated field names, for `dev/` debugging and for anyone reading
   `vec_data()`. Only worth adding if the readability itch (rather than memory) was the real driver.
   Default answer: **no** — it is a helper with one caller, which is itself an ad hoc layer.
4. **Threshold for re-opening.** This verdict is a function of *20 fields / 21 constructor calls per
   build*. If a future phase pushes the record past ~30 fields (Phase z7's gap standard error would
   be the 21st), re-run §4 and §5 before assuming the answer still holds. Record the numbers here
   rather than re-deriving them.

---

## Appendix — reproduction

All figures above come from throwaway scripts against `devtools::load_all("~/github/tabxplor")` on
WSL2/ext4, `OMP_NUM_THREADS=1`, single session, nothing else running. Method notes that matter if
these are re-measured:

* **Benchmark harness must `eval()` the expression each iteration.** A first attempt used
  `for (i in 1:200) force(e)`, which evaluates the promise once and then measures 199 no-ops — it
  produced plausible-looking, entirely meaningless numbers.
* `new_fmt` is a locked binding; instrumenting it needs `unlockBinding("new_fmt", asNamespace("tabxplor"))`.
* `object.size()` does **not** deduplicate shared SEXPs, so it cannot see the §5 sharing effect —
  that measurement used `gc()`-delta on `Vcells`, and SEXP identity was checked by parsing
  `.Internal(inspect(x))` addresses (`lobstr` is not installed here).
* Timings vary ±15 % run to run on this box; only ratios within a single run are meaningful, which is
  why §4's end-to-end comparison is reported as "no visible effect" rather than as a number.
