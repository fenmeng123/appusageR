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
