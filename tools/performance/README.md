# Reproducing the 0.3.5 comparison

Performance accepted by the user on 2026-10-03 after 15 successful latest-only
runs, package check 0/0/0, 40 existing-output comparisons and 100 repeatability
comparisons. The raw05 old-output gap remains disclosed. Private data/results
remain outside Git. The scripts below preserve reproducibility; completed runs
are not a queue to restart automatically.

## Completed measurement protocol, revised 2026-09-30

The user cancelled old-version validation and performance measurement, and
authorized the additional QC context/date reuse. **Do not run the old campaign
or restart its raw05 baseline.** The older instructions below are historical.

1. Run package regression/check, `verify_qc_context_035.R PRIVATE_WORK_ROOT`,
   text-call audit and the two-worker smoke test. QC verification reads the
   frozen pre-addition 0.3.5 source and existing raw01-raw04 artifacts only;
   it performs correctness comparisons and does not collect old-code timings.
2. Freeze with `freeze_qc_plus_035.py --candidate candidate-plus-v2`; install
   that snapshot into the matching `candidate-plus-v2-lib`. Failed or earlier
   snapshots remain preserved. Never overwrite a measured snapshot/library.
3. Run `latest_campaign_035.py --work PRIVATE_WORK_ROOT` after validation.
   It runs only the latest candidate, three rounds of raw01-raw05, one R process
   and worker per file, with fresh outputs and the existing external monitor.
4. Run `report_latest_035.py --work PRIVATE_WORK_ROOT`. Compare completed
   latest artifacts to the four already available old outputs and compare
   repeated latest outputs using `compare_latest_runs.R`; do not rerun the old parser.

The original comparator and initial runtime-only mismatch evidence are retained.
The final comparator explicitly names three additional host measurement fields
under `first_level_worker_decision`: `detected_total_memory_bytes`,
`memory_detected`, `memory_source`. Worker decisions and source/configuration
fingerprints remain strict. Unnamed arrays of plain scalars are preserved
directly, avoiding expensive scalar recursion without changing value/type/order
comparison. `comparisons-final` contains the final audit results.

The latest campaign retains source/library/tool hashes, dependency versions,
source hashes and full timing/memory records. `latest-only/measurements.csv`
contains all 15 observations; `table.md` contains medians/ranges. Correctness
comparisons take place outside measured runs. No speedup ratio is required.
Raw05 has no complete old output because the user stopped that validation.

## Historical protocol (cancelled; not dispatchable)

These development tools are excluded from the built R package. Production code
is under `R/`. Old base/stringr/iconv calls here are comparison oracles, not
production fallback engines. Source paths and complete artifacts belong only in
the private workflow-test directory; public reports identify raw01–raw05 only.

Use Windows x64 R 4.5.3, stringi 1.8.7, ICU 74.1 and `Chinese_China.utf8`.
Every R process uses its explicitly supplied installed library. The old source
is commit `26c4d7e8b7f2dfa4b8b65c5bb0d61554299633f1`.

1. Freeze the authorized source manifest with `benchmark_id`, `source_file`,
   `size_bytes`, `source_md5`; preserve raw01–raw05 identities. Never rediscover
   corpus sources automatically. Install the old snapshot in `baseline-lib`.
2. Run `supervise.py --library BASELINE_LIB --manifest PRIVATE_MANIFEST
   --output GOLDEN_ROOT --role baseline`. This produces the complete old oracle
   without a wall-clock timeout. Development-concurrent oracle timings are not
   formal speed measurements. A failed/incomplete directory is never overwritten.
3. Use `differential_structure.R`, `differential_text.R`, the test suite and
   `audit_text_calls.R` for development regression checks. Finite Windows codec
   and case compatibility deltas can be regenerated using the `freeze_*_compat.R`
   tools. The frozen structural snapshot retains the old text engines.
4. `freeze_final_035.py` freezes the final candidate and updated structural
   attribution sources. Install each in its own library. Run `smoke_workers.R
   CANDIDATE_LIB PRIVATE_OUTPUT`, synthetic `benchmark_one.R` entries and
   `compare_runs.R OLD_RUN NEW_RUN PRIVATE_REPORT`. Run the approved explicit
   encoding reproduction with `verify_encoding_exception.R PRIVATE_WORK_ROOT`.
5. Run `devtools::document(); devtools::test()` via `test_all.R` and the package
   check. If the Windows processx named-pipe launcher fails, use the same R's
   direct `R.exe CMD build .` and `R.exe CMD check --no-manual`.
   `record_validation.R PRIVATE_WORK_ROOT` records counts and shared dependencies.
   `prepare_measurement_gate.py` verifies all evidence and hashes libraries and
   tools. It refuses to replace an existing readiness record.
6. Inspect `campaign.py --config PRIVATE_CONFIG --dry-run`, then run without
   `--dry-run`. The config supplies private manifest, three libraries, old oracle,
   empty campaign root, readiness file and public report directory. The campaign
   waits for the complete oracle/readiness, then runs five intermediate
   observations, five final equivalence runs and thirty formal runs, serially.
   The order alternates within each old/new file pair by repetition and file.

The supervisor has no wall-clock deadline. It monitors the actual x64 R child
and host memory every second; sustained available memory below 4 GiB for 30
observations ends its own process tree as a resource failure. Process failures
and incomplete repetitions do not yield speed ratios. Exact comparison failures
stop the campaign for repair/review. Existing successful outputs can be rechecked
on resume; incomplete output directories are preserved as evidence.

API wall time starts at the first-level entry and ends after second-level,
inline QC and cache/summary output. External process wall includes startup and
verification. CPU, OS peak working set, sampled private memory, host minimum
available memory and recorded substage times are retained. Source hashing warms
the OS cache; these are not cold-cache claims. No other performance experiment
should run during the formal campaign.

The artifact comparator retains source/config fingerprints, scientific values,
types, order, attributes and QC. Only explicit version/implementation/run/time/
measurement/output-root/backend wording fields are normalized, with an audit
log. The approved explicit-source-encoding duplicate-transcoding fix is tested
separately; it does not broaden real-source comparison allowances.

Public reports contain raw timings, stage fractions, memory, medians/ranges and
comparison status. Structural timings are one observation per file, not formal
three-repeat estimates. The five largest sources describe the long tail only.
Completion means waiting for user performance acceptance; no release or real
project migration is performed by these tools.
