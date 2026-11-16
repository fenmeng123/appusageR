# appusageR 0.3.11

- Serial file processing omits host-memory subprocesses that cannot change its
  one-worker selection, recording the measurement as not requested. Parallel
  decisions continue to measure available memory and apply the existing caps.

- Calendar-date conversion consistently maps nonfinite Date, POSIXct, POSIXlt
  and millisecond inputs to missing dates across R versions. Finite timestamp
  conversion and scientific rules are unchanged.

- Collection resume accepts all-failure summaries whose missing cache paths are
  read from CSV as logical values. Complete diagnosed failures are reused with
  their source-issue counts; existing scientific-artifact protections remain.

- Expand documentation with a shared fictional project and the portable
  `appusage_demo` dataset. All 61 public functions have runnable examples;
  `appusage_configuration` documents 46 workflow settings and five category
  policy arguments. Teaching examples use temporary caches and one worker.
  Package-level help and six runnable task tutorials connect these examples,
  including category conflicts, paired-cache repair, an all-failure project
  and questionnaire relationships.

- Batch category enrichment now dispatches every valid proc-2 path. Recovery
  recognizes empty mixed exports with complete preflight failure evidence while
  continuing to reject incomplete parses and damaged caches.

- Scientific output schema 0.3.5 retains meta table 2 events alongside native
  line episodes and daily totals in mixed exports. New proc-1 and proc-2 stage
  contracts record their schema version. Older files retain their creator metadata.

- The bundled dictionary now records its version as `2.0.0`. Its five columns,
  all labels and the historical source identifiers are identical to the previous snapshot.

- `audit_appusage_days()` reports native date-block evidence and recorded
  duration using explicit day denominators. `audit_appusage_overlap()`
  reports five half-open interval relations, local-day coverage, and optional
  app allocations. See the vignette *Day coverage and overlapping intervals*.
- Daily overlap QC and its report projections preserve exact pair counts
  above the 32-bit integer limit.

# appusageR 0.3.10 (2026-10-27)

- Rewrite DESCRIPTION and the package README around processing capabilities,
  workflow examples and dictionary use. Keep dictionary version v2.0.0 and
  scientific output schema 0.3.4.

- Shorten temporary and backup names for paired cache transactions on Windows,
  while retaining final cache names, rollback and legacy recovery discovery.
  Preserve the original processing error if writing its status metadata also
  fails. Regression tests exercise long paths and both failure conditions.
- Reduce repeated fingerprint serialization without changing fingerprint values:
  use file-backed object serialization and reuse function text only after checking
  its current definition. Keep dependency traversal and invalidation checks active.
  Reuse a resolved dependency digest only after rechecking every observed namespace
  binding, including symbols that were previously not functions.
  Clarify the cache description for CRAN's incoming spelling check.
- Give synthetic CRAN tests a deterministic memory fixture instead of repeatedly
  launching system probes. Retain a real detector test, low/unknown-memory and
  recovery tests, and unchanged real-memory local eight-worker acceptance.

# appusageR 0.3.9

- Prepare CRAN checks with a short two-worker category equivalence test; retain
  the full eight-worker/64-file acceptance test for local runs. Category workers
  inherit the caller's locale instead of imposing a Windows-only locale name.
  Clarify GPL-3 and author-confirmed dictionary copyright/contributor roles,
  declare R >= 4.1.0 and the test helper requirements, and add runnable synthetic
  category examples. Dictionary v2.0.0 bytes and scientific rules are unchanged.
  Encoding regression tests use the frozen Windows R 4.5.3 reference values on
  all platforms, rather than assuming each host's iconv has the same mappings.

- Adopt independent dictionary semantic versions: v261013 is v1.0.0 and
  v261019 is v2.0.0. Only provenance version metadata changes; all dictionary
  cells are preserved. See installed DICTIONARY_VERSIONING.md for increment rules.

- Update the independently versioned dictionary to v261019 (2026-10-19), keeping
  the package version at 0.3.9. Preserve all 11,849 corrected v261013 rows;
  append 16,239 alias rows for 14,118 identities, with 1,925 user-approved
  high/medium functional labels and 12,193 provisional Others / Unclassified
  identities. Keep the original five public columns and GPL-3 attribution.
- Complete optional categories with bundled v261013 data, APK-first matching and
  exact name disambiguation restricted to conflicting APK candidates. Preserve
  unresolved conflicts and record package/name disagreement without overriding APK.
- Add explicit-person unknown-app profiles: maximum primary-file foreground share
  and positive-foreground person prevalence; 5% OR 1% triggers manual review.
  Measured low-priority unknowns can receive operational Others / Unclassified;
  these are not additions to the manual 14/31 functional taxonomy.
- Add transactional category-only updates for project caches and flat copied
  RDA/JSON pairs, integrity-checked reuse, bounded checkpoints and worker fallback.
- Distinguish matched, residual, pending, missing-metric and non-app dispositions.
  Extract unknown identities across grains while measuring one canonical table;
  event, episode and daily durations are never summed together.
- Count distinct source apps once across all grains; per-grain appearances remain
  in the grain summary and are not presented as distinct applications.
- Keep scientific schema 0.3.4, source/QC/relationship rules and non-category
  scientific data unchanged. DataBox V4 delivery preserves V3 and root files.

# appusageR 0.3.8 (2026-10-12)

- Apply the existing file-reader NUL cleanup during preflight after the binary
  gates. Sparse-NUL non-APP Usage logs retain an unknown-content diagnosis;
  original byte counts and ratios remain available for inspection.
- Recognize ISO base media (`ftyp`) uploads before text decoding, including
  compressed videos with NUL/control-byte ratios below the existing thresholds.
  Retain these sources as diagnosed binary-input failures.
- Validate decimal and exponent spellings of ordinary numeric Excel cells by
  exact numeric value. Preserve literal character/date/time cell checks and
  continue to reject changed values and missing cells.
- Return `NA` for invalid APP Usage date tokens without aborting type detection.
  Unknown text containing malformed date-like values keeps its source diagnosis;
  valid calendar dates and scientific processing rules remain unchanged.
- Add `run_appusage_projects()` for serial project collections with configurable
  file workers, compact checkpoints, failure isolation and completed-project reuse.
  Validate registry identities, directories and questionnaire columns before execution.
- Add read-only `validate_appusage_project()` with metadata previews and content
  checks for source inventories, stage ownership, RDA types, links and Excel cells.
  Retain diagnosed input failures in completed delivery reports.
- Pass the existing worker cap override from the collection runner. Collection
  runs select eight file workers by default and retain existing retry behavior.
  Protect accepted outputs with a `read_only` registry policy and share compact
  metadata between planning and inspection. Scientific schema remains 0.3.4.
- Preserve saved diagnostic columns when summary CSV and Excel are both missing;
  restore projections from intact caches without scientific recomputation.
  Missing second-level summaries produce explicit checks without column warnings.
- Reconstruct missing first-summary metadata in memory before checking scientific
  cache protection. Unverified or unidentified existing payloads require explicit
  rebuild approval; compatible summary recovery keeps scientific artifacts intact.

# appusageR 0.3.7 (2026-10-06)

Development and performance acceptance completed on 2026-10-06.

- Explain source and project work with stable task IDs, reason codes, configuration
  differences and explicit verification scope. Execution revalidates saved plans
  and records planned/actual actions, outcome, deviations and stage timings.
- Share matching, LinkResult reuse and CSV/Excel projection between the generic
  workflow, Wenjuanxing adapter and independent stages. Missing exports can be
  rebuilt without rematching; failed and unselected sources remain visible.
- Reuse run provenance, compare canonical contracts without temporary hash files,
  share metadata within validation operations and skip unchanged projections.
  Content integrity checks and checkpoint recovery remain enabled.
- Reuse decoded summaries and compact validation metadata through the existing
  project control file, checking actual owner bytes on every access. Discard
  damaged/incompatible indexes and invalidate entries after writes. Reuse an
  intact unchanged research batch without per-source result allocation, and
  avoid rereading an unchanged questionnaire workbook during project resumes.
- Add compact `print()` / `summary()` inspection without implicit file access.
  Separate execution, artifact verification, coverage QC, available grains and
  analysis eligibility. Optional internal diagnostics count calls and bytes;
  formal timing uses uninstrumented execution.
- Preserve scientific schema 0.3.4 and existing scientific rules. Duration-label
  refresh retains numerical research columns and category annotations.
- Validate the final candidate with 2,057 passing assertions, installation,
  two-worker smoke tests, and package check with 0 errors, 0 warnings and 0 notes.
  The check used independently verified host UTC, with
  `_R_CHECK_SYSTEM_CLOCK_=FALSE` and `_R_CHECK_FUTURE_FILE_TIMESTAMPS_=TRUE`,
  after the online clock service failed. File timestamp checks remained enabled;
  the earlier external-clock NOTE is preserved in the development records.
- Complete three isolated real-project matrices covering 4,802 sources:
  4,388 completed through research-data processing and QC execution, while 414
  retained diagnosed upstream failures. Full audits passed for 8,776 RDA,
  9,190 JSON and 3,831,894 exported Excel cells. Nine unchanged resumes,
  per-project repair and summary rebuilds, four configuration-change scenarios,
  and all 4,805 final input hashes passed. Diagnostic resumes performed zero
  scientific calculations, RDA loads, matching calculations or full JSON/CSV
  decoding.
- Record unchanged-resume external wall-clock medians of 86.6966, 140.2423
  and 432.9784 seconds for 559, 1,162 and 3,081 sources. The large-project
  median fell from 1,988.2377 to 432.9784 seconds within this iteration
  (78.223% less elapsed time), using existing measurements of the earlier
  candidate. OS file-cache conditions were uncontrolled. Index construction
  was included in fresh runs; missing-index fallback was not timed separately
  on the large project.
- Accept 59 changes in implementation traceback text following direct user
  review on 2026-10-05. Preserve the original comparison evidence and retain
  scientific, QC, identity, content-integrity and recovery validation.
- Consolidate the repository overview into a directly maintained `README.md`,
  with an architecture diagram and module API summaries. Keep `README.Rmd`
  and development/monitoring/acceptance tools local and excluded from Git and
  package builds. Restore the previously approved package logo from its
  original design conversation.

# appusageR 0.3.6 (2026-10-03)

- Add a generic manifest workflow, validated `appusage_config()`, read-only
  `plan_appusage_workflow()` and independently callable `run_appusage_stage()`.
  The Wenjuanxing project workflow remains a compatible optional adapter.
- Share single-source parsing and workflow execution. Honor effective timezone,
  parser strictness and QC configuration consistently across entry points.
- Expose `standardize_appusage()`, `build_appusage_daily()`, complete in-memory
  `assess_appusage_qc()` and `match_appusage_self_report()`. Existing raw parsers
  and explicit meta reconstruction retain their scientific contracts.
- Validate cache reuse per source and stage using configuration, implementation,
  upstream dependencies and artifact integrity; repair partial/corrupt sources
  and support raw-free downstream execution from compatible caches.
- Persist questionnaire relationship results and project summaries through a
  common projection. Preserve unselected sources, failures and matching fields
  across QC, category and targeted rebuild operations. Overwrite no longer
  deletes the complete project directory.
- Keep schema 0.3.4, two main cache levels and the existing parallel scheduler.
  R 4.5.3 validation: 1809 passing assertions and package check 0/0/0. One
  isolated 559-source project completed with 500 successes through QC and 59
  explained input failures; all 1000 RDA/1059 JSON artifacts, questionnaire
  exports and unchanged/targeted recovery audits passed. The user accepted 0.3.6
  on 2026-10-03 and authorized Git commit/push. Broader migration and tagged or
  package publication remain separate decisions.

# appusageR 0.3.5 (2026-10-03)

- Share a file-local ragged parsing context across preflight, detection, table
  boundaries, parsing, and diagnostics. Build parsed tables by columns.
- Use indexed meta event pairing, grouped episode merging, batch timeline
  clipping, compact day-segment indices, and reusable date/midnight conversion.
- Share call-local QC column, row-selection, validity and date caches across
  anomaly, timestamp, overlap and export-span checks. Distinguish timezone and
  midnight endpoint conventions; preserve all QC decisions and output fields.
- Route data-text operations through stringi, remove the direct stringr
  dependency, and retain the prior NA, delimiter, case and codec contracts.
  ICU compatibility corrections preserve the frozen Windows converter behavior;
  platform encoding-name discovery remains metadata-only.
- Fix duplicate transcoding when an explicit source encoding is supplied after
  preflight or when an already decoded `appusage_text` enters preprocessing.
  GBK, GB18030 and CP936 regression cases cover all four export formats. This
  narrowly specified compatibility exception was approved on 2026-09-29.
- Include the new parsing, time, interval and text helpers in implementation
  fingerprints. Public API signatures and output schema 0.3.4 remain unchanged.

Performance was accepted by the user on 2026-10-03 after all 15 latest-version
benchmark runs succeeded. R 4.5.3 package check completed with 0 errors,
0 warnings and 0 notes. Four existing old-output comparisons (40 artifacts)
and repeated latest-output comparisons (100 artifacts) passed. The cancelled
raw05 old run leaves an explicit old/new equivalence gap. Historical-output
migration and a tagged or CRAN release are separate decisions.

# appusageR 0.3.4 (2026-07-12)

## Compatibility hardening

- Added deterministic per-source cache identity and explicit cache-pair resume
  states while preserving unambiguous legacy cache readability.
- Made content detection authoritative for supported exports, added strict
  mixed-section boundaries, and retained `Unlock` as an unsupported export.
- Standardized all datetime-derived dates on an explicit default timezone of
  `Asia/Shanghai` and added duration-conserving local-midnight segmentation.
- Added deterministic daily ordering, daily aggregation self-checks, and
  source-anomaly QC with flags and eligibility rather than row deletion.

## Provenance and rebuild planning

- Workflow configurations, proc-1/proc-2 JSON metadata, summaries, checkpoints,
  and diagnostics now carry coherent implementation provenance.
- Added `plan_appusage_project_rebuild()` for metadata-only, non-mutating
  previews of targeted retry, reconciliation, second-level rebuild, QC refresh,
  summary-cardinality refresh, and matching-refresh needs.

Package-code acceptance completed on 2026-07-12 at commit `e2837db`.
Historical-project output migration remained pending.

# appusageR 0.3.3 (2026-07-09)

- Recover first-level summaries from existing per-source JSON/RDA caches and
  resume interrupted projects when the final summary is missing.
- Add chunk checkpoints, memory-aware worker selection and reduced-worker or
  serial retries for memory-allocation failures.
- Vectorize line episode-to-daily aggregation, run routine QC inline, and use
  size-aware second-level scheduling and load-balanced first-level processing.
- Refresh summaries and matching by stable source keys. Collect structured
  worker progress, errors, diagnostic paths and retry outcomes in the parent
  process while retaining source order.
- Complete the preprocessing-speed work carried forward from 0.3.2.

# appusageR 0.3.2

- Clip reconstructed meta episodes into a global foreground timeline, retain
  source-duration and episode-count audit fields, and cover episode-derived
  daily aggregation with regression tests.
- Add `rerun_second_level_project_subset()` for configuration-backed targeted
  second-level rebuilds, preserving unselected rows and self-report fields.
- Carry remaining speed-refactoring work into 0.3.3. The earlier 0.3.1 scope
  remained a development plan; its structured-progress and project-validation
  work was absorbed into later workflow iterations.

# appusageR 0.3.0 (2026-06-30)

- Add project-level Wenjuanxing matching using questionnaire sequence IDs and
  uploaded filenames, including complex upload-URL extraction.
- Resolve duplicate uploads by export-type priority and export/submit-time
  proximity. Preserve unmatched questionnaire rows in matched Excel output.
- Support matching-only recovery and relative `moSens_data_dir` references to
  research caches. Diagnose unsupported Unlock exports at first level.

# appusageR 0.2.10 (2026-06-24)

- Add project manifests, project orchestration and actionable batch-error
  diagnostics ahead of the self-report workflow.

# appusageR 0.2.2–0.2.9 (2026-06-22)

- Stabilize text parsing and module boundaries, unify research schemas and
  document foreground/background flags.
- Add explicit meta episode reconstruction, daily-source options and anomaly
  QC. These iterations were accepted together on 2026-06-22.
