# Phase 23e — test-suite timings

`phase23e_test_timings_before.csv` — per file, before the phase (104 files). ⚠ Measured with a
harness that re-sourced `setup.R` per file; `setDTthreads()` rebuilds data.table's OpenMP pool on
every call, so the ABSOLUTE numbers are inflated ~2x. The ranking is sound; the totals are not.

`phase23e_test_timings_after.txt` — per file, after (46 files), each timed alone through
`testthat::test_local(filter=)`. Includes ~2 s of worker startup per line.

The honest end-to-end figures, `devtools::test()` on this box (6 physical cores / 12 SMT):

| | files | assertions | wall (6 workers) | serial |
|:--|--:|--:|--:|--:|
| before | 104 | 9 273 | 134 s | — |
| after  |  46 | 4 316 |  ~40 s | 136 s |
| dev suite | 41 | 5 899 | — | — |

⚠ Parallel scaling is capped by PHYSICAL cores: `detectCores()` reports 12 here but the CPU has 6
(`thread_siblings_list` shows 2 siblings per core). 8 workers beat 6 by only ~6 % while
oversubscribing a shared machine, so `tests/testthat.R` now sizes the pool from the kernel topology.
