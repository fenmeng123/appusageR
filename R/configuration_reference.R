#' Workflow configuration parameter reference
#'
#' Defaults, units and scope of all settings accepted by [appusage_config()].
#' Pass named lists for the sections you want to change; omitted fields retain
#' their defaults. This reference describes the existing scientific and
#' execution controls.
#'
#' @section Parsing and time:
#' `parse` determines faithful source reading; `time` determines local dates
#' and interval boundaries. Strings below are character scalars.
#'
#' | Setting | Default |
#' |---|---|
#' | `parse$type` | `"auto"` |
#' | `parse$input` | `"file"` |
#' | `parse$encoding` | `"auto"` |
#' | `parse$parser_strict` | `TRUE` |
#' | `time$tz` | `"Asia/Shanghai"` |
#'
#' - `parse$type`: Content selection: `auto`, `line`, `meta`, `day`, `app`, or `legacy`. Auto retains supported mixed line/meta blocks; legacy selects one component.
#' - `parse$input`: `file`, `text`, or `lines`. Project/cache workflows use file inputs; text/lines are useful with the in-memory readers.
#' - `parse$encoding`: Automatic decoding or a nonempty source-encoding name, e.g. `"UTF-8"`.
#' - `parse$parser_strict`: Logical: malformed parser structures produce an error condition. Batch continuation is controlled separately by `execution$strict`.
#' - `time$tz`: Valid timezone used for timestamp-derived dates and midnight boundaries.
#'
#' @section Reconstruction and daily tables:
#' Numeric durations are milliseconds. Event-type vectors contain Android
#' UsageEvents codes. Numeric configuration values must be finite and
#' nonnegative; scalar settings accept one value.
#'
#' | Setting | Default |
#' |---|---|
#' | `reconstruction$include_collection_app` | `TRUE` |
#' | `reconstruction$reconstruct_meta` | `TRUE` |
#' | `reconstruction$meta_pairing` | `"package"` |
#' | `reconstruction$meta_start_event_types` | `1` |
#' | `reconstruction$meta_end_event_types` | `c(2, 23)` |
#' | `reconstruction$merge_meta_episodes` | `TRUE` |
#' | `reconstruction$meta_episode_merge_gap_ms` | `30000` |
#' | `daily$meta_daily_source` | `"summary"` |
#'
#' - `reconstruction$include_collection_app`: Logical: retain `com.w.appusage` rows in research-data tables.
#' - `reconstruction$reconstruct_meta`: Logical: reconstruct pure meta events into episodes. Mixed behavior depends on whether this argument was explicitly supplied; see below.
#' - `reconstruction$meta_pairing`: Pair by `package` or `package_class`.
#' - `reconstruction$meta_start_event_types`: Numeric vector of episode-start event codes.
#' - `reconstruction$meta_end_event_types`: Numeric vector of episode-end event codes.
#' - `reconstruction$merge_meta_episodes`: Logical: merge compatible adjacent reconstructed meta episodes.
#' - `reconstruction$meta_episode_merge_gap_ms`: Maximum merging gap in milliseconds (30 seconds).
#' - `daily$meta_daily_source`: `summary` uses Table 1; `episodes` uses reconstructed intervals; `both` retains both with source labels. Select one representation when calculating totals.
#'
#' Mixed sources use native line episodes/daily totals and meta Table 2 events
#' when meta options are omitted. For mixed input, omit `meta_daily_source`
#' and omit `reconstruct_meta` or explicitly set the latter to `FALSE`.
#' For explicit meta reconstruction select `parse = list(type = "meta")`.
#' Episode-derived daily modes require reconstruction. The configuration stores
#' whether those two options were explicitly supplied; editing an expanded
#' saved list can lose that distinction. Create a new configuration with
#' [appusage_config()]. Old configurations without the explicitness record
#' retain legacy single-component selection.
#'
#' @section Routine QC and anomaly thresholds:
#' These settings determine flags/coverage decisions rather than deleting
#' scientific rows. QC execution success, coverage pass and grain eligibility
#' are separate outputs. The following are the defaults, not recommendations
#' to change study criteria. See [qc_appusage_day()] and [assess_appusage_qc()].
#'
#' | Setting | Default |
#' |---|---|
#' | `qc$enabled` | `TRUE` |
#' | `qc$require_all_weekdays` | `TRUE` |
#' | `qc$min_nonempty_days` | `7` |
#' | `qc$use_all_apps_row` | `FALSE` |
#' | `qc$drop_likely_total_all_rows` | `TRUE` |
#' | `qc$all_row_tolerance` | `0.10` |
#' | `qc$max_episode_ms` | `86400000` |
#' | `qc$max_daily_app_ms` | `86400000` |
#' | `qc$max_daily_total_ms` | `86400000` |
#' | `qc$max_export_lookback_days` | `31` |
#' | `qc$meta_diff_abs_ms` | `60000` |
#' | `qc$meta_diff_ratio` | `0.20` |
#'
#' - `qc$enabled`: Logical: execute routine source QC.
#' - `qc$require_all_weekdays`: Logical: require Monday through Sunday among nonempty dates.
#' - `qc$min_nonempty_days`: Minimum count of days with positive valid use.
#' - `qc$use_all_apps_row`: Logical: use the exported ALL total for daily QC instead of summing application rows.
#' - `qc$drop_likely_total_all_rows`: Logical: omit likely multi-day ALL totals from daily QC.
#' - `qc$all_row_tolerance`: Relative tolerance used to recognize a likely ALL total.
#' - `qc$max_episode_ms`: Episode-duration anomaly threshold, milliseconds (24 hours).
#' - `qc$max_daily_app_ms`: App/day-duration anomaly threshold, milliseconds.
#' - `qc$max_daily_total_ms`: Summed source/day-duration anomaly threshold, milliseconds.
#' - `qc$max_export_lookback_days`: Expected maximum record-to-export lookback, days.
#' - `qc$meta_diff_abs_ms`: Absolute meta summary/episode discrepancy threshold, milliseconds.
#' - `qc$meta_diff_ratio`: Relative meta summary/episode discrepancy threshold.
#'
#' @section Optional categories:
#' Classification is independent of scientific reconstruction and routine QC.
#' A dictionary update refreshes classification for an explicitly requested run.
#'
#' | Setting | Default |
#' |---|---|
#' | `category$enabled` | `FALSE` |
#' | `category$dictionary` | `NULL` |
#' | `category$overwrite` | `TRUE` |
#' | `category$policy` | `appusage_category_policy()` |
#' | `category$profile` | `NULL` |
#' | `category$manifest` | `NULL` |
#'
#' - `category$enabled`: Logical: attach category labels.
#' - `category$dictionary`: Bundled dictionary; alternatively a dictionary data frame or Excel path.
#' - `category$overwrite`: Logical: replace existing nonmissing category values.
#' - `category$policy`: Validated classification/review policy from its constructor.
#' - `category$profile`: Optional frozen result of `profile_uncoded_apps()`.
#' - `category$manifest`: Optional explicit source/person/observation/primary-source manifest for profiling.
#'
#' Construct `category$policy` with the following five arguments. The default
#' policy version is `"category-policy-1"`; it is separate from both the
#' manual dictionary version and the package version.
#'
#' | Setting | Default |
#' |---|---|
#' | `share_threshold` | `0.05` |
#' | `prevalence_threshold` | `0.01` |
#' | `auto_other` | `TRUE` |
#' | `other_l1` | `"Others"` |
#' | `other_l2` | `"Unclassified"` |
#'
#' - `share_threshold`: Proportion from 0 to 1 inclusive; a person's maximum primary-file foreground share at or above this value triggers review.
#' - `prevalence_threshold`: Proportion from 0 to 1 inclusive; positive-foreground person prevalence at or above this value triggers review.
#' - `auto_other`: Logical: assign the residual pair to measurable unknown apps below both thresholds.
#' - `other_l1`: Nonempty character residual L1 label.
#' - `other_l2`: Nonempty character residual L2 label.
#'
#' Thresholds use inclusive OR logic. The fixed policy uses a person's maximum
#' share across primary observations and positive-foreground presence across
#' people. Missing metrics remain pending. A manifest identifies primary files,
#' shared observations and people explicitly; file counts are not person counts.
#' Dictionary matches and residual assignments are reported separately.
#'
#' @section Questionnaire matching:
#' Matching preserves every questionnaire row. Column-name settings are
#' character scalars.
#'
#' [preflight_self_report_matching()] checks the table
#' interface before [match_appusage_self_report()] resolves uploads.
#'
#' | Setting | Default |
#' |---|---|
#' | `matching$enabled` | `FALSE` |
#' | `matching$self_report` | `NULL` |
#' | `matching$self_report_file` | `NULL` |
#' | `matching$sequence_col` | `"\u5e8f\u53f7"` |
#' | `matching$upload_col` | `NULL` |
#' | `matching$submit_time_col` | `NULL` |
#' | `matching$export_type_priority` | `c("line", "meta", "day", "app")` |
#'
#' - `matching$enabled`: Logical: link questionnaire rows to cache references.
#' - `matching$self_report`: In-memory questionnaire data frame.
#' - `matching$self_report_file`: Optional questionnaire workbook path.
#' - `matching$sequence_col`: Original platform-sequence column; synthetic survey uses `"sequence_id"`.
#' - `matching$upload_col`: Upload filename column; required when matching is enabled. Synthetic survey uses `"upload"`.
#' - `matching$submit_time_col`: Optional submission-time column for candidate selection; synthetic survey uses `"submitted_at"`.
#' - `matching$export_type_priority`: Unique supported export types in descending selection priority.
#'
#' Workbook sheet/type-reading controls belong to the project adapter arguments
#' of [run_appusage_project_workflow()] or the collection registry accepted by
#' [run_appusage_projects()]. They are not extra `matching` configuration fields.
#'
#' @section Execution and recovery:
#' Execution settings control resources, continuation and cache verification.
#' They are distinct from scientific criteria.
#'
#' | Setting | Default |
#' |---|---|
#' | `execution$parallel` | `FALSE` |
#' | `execution$workers` | `1L` |
#' | `execution$strict` | `FALSE` |
#' | `execution$progress` | `TRUE` |
#' | `execution$resume` | `TRUE` |
#' | `execution$overwrite` | `FALSE` |
#' | `execution$checkpoint_every` | `100L` |
#' | `execution$source_verification` | `"content"` |
#'
#' - `execution$parallel`: Logical: request file-level parallel work.
#' - `execution$workers`: Positive integer file-worker request; runtime resource limits may reduce actual workers.
#' - `execution$strict`: Logical: stop the batch/workflow on a failed source when TRUE; otherwise retain its diagnostic row and continue.
#' - `execution$progress`: Logical: print progress messages.
#' - `execution$resume`: Logical: reuse compatible, verified existing work.
#' - `execution$overwrite`: Logical: allow requested replacement; ordinary unchanged resume uses FALSE.
#' - `execution$checkpoint_every`: Positive integer source-record interval for checkpoints.
#' - `execution$source_verification`: `content` checks source content; `metadata` uses source size/mtime; `cache_only` uses saved caches without raw-source comparison.
#'
#' Planning with `verify = "metadata"` is a metadata preview. Use
#' [validate_appusage_project()] with `verify = "content"` for content delivery
#' checks. An invalid upstream cache in cache-only mode is blocked rather than
#' reparsed. In lower-level batch APIs the worker argument is `n_cores`;
#' [run_appusage_projects()] uses `workers` (default eight) while executing
#' projects serially. The examples use one worker; the installed
#' `demo("synthetic-project", package = "appusageR")` demonstrates parallel work.
#'
#' @section Configuration and stage ownership:
#' Pass `config` to the workflow instead of simultaneously supplying legacy
#' scientific/execution arguments. Unknown fields are rejected. A saved
#' configuration contains requested settings; the plan and `run_record` contain
#' proposed and actual actions. Parsing changes affect downstream work;
#' reconstruction/daily changes affect research-data and dependent stages;
#' coverage-QC and category changes have their own stage contracts. Duration
#' thresholds also synchronize persisted anomaly labels. Source selection,
#' relationship selection and category coverage are separate concerns.
#'
#' Functions also document their own controls, such as `verify` in validation,
#' `strategy`/`window` options in [audit_appusage_days()], and
#' `source_qc_config` in [qc_appusage_anomalies()]. Those are not additional
#' fields accepted by [appusage_config()].
#'
#' @seealso [appusage_config()], [appusage_category_policy()],
#'   [appusage_demo], [run_appusage_stage()]
#' @examples
#' data(appusage_demo)
#' config <- appusage_config(
#'   time = list(tz = appusage_demo$info$timezone),
#'   execution = list(parallel = FALSE, workers = 1L, progress = FALSE))
#' names(config)
#' stopifnot(config$qc$min_nonempty_days == 7,
#'           config$category$policy$share_threshold == 0.05)
#' # Supply the synthetic questionnaire and its explicit column names.
#' linked_config <- appusage_config(matching = list(
#'   enabled = TRUE, self_report = appusage_demo$survey,
#'   sequence_col = "sequence_id", upload_col = "upload",
#'   submit_time_col = "submitted_at"))
#' stopifnot(nrow(linked_config$matching$self_report) == 17L)
#' @name appusage_configuration
NULL
