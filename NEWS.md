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

# appusageR 0.3.4

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

Real-data rebuild acceptance is intentionally separate from this package-code
release and is not implied by these changes.
