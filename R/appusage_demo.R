#' Standardized fictional APP Usage project and questionnaire
#'
#' A portable, in-memory version of the synthetic ProjectID `"009"`. Ten
#' successful sources retain their separate standardized tables and coverage
#' metadata. Five deliberate input failures remain in the source inventory.
#'
#' @format A named list with five elements:
#' \describe{
#'   \item{info}{ProjectID, scientific schema `0.3.5`, timezone
#'     `Asia/Shanghai`, relative source root, fictional origin and GPL-3 license.}
#'   \item{sources}{A 15-row data frame describing each source, its relative
#'     TXT path, character upload sequence, scenario, independently specified
#'     expected status, error class, grain counts, duration and coverage QC.}
#'   \item{records}{Ten named `appusage_second_level` lists, keyed by the
#'     scenario in `sources`. Each contains `status`, `data`, `data_file`,
#'     `metadata_file` and `metadata`. Cache paths are `NA_character_`.
#'     `data` contains typed `event`, `episode` and `daily` tibbles with their
#'     scientific attributes. `metadata` holds export and day-coverage facts.}
#'   \item{survey}{17 fictional questionnaire rows. `sequence_id` and
#'     `survey_id` are character; `age_years`, `sleep_hours` and
#'     `phone_minutes_day` are numeric; `upload` and `submitted_at` are
#'     character. Submission strings denote Asia/Shanghai wall time.
#'     Missing sleep/upload values are `NA`; zero phone minutes is observed.}
#'   \item{expected_links}{17 rows giving expected full-project matching
#'     statuses and selected source basenames, in questionnaire order.}
#' }
#' @details
#' Records cover line, meta, day, app and mixed line/meta exports; recorded,
#' explicit-empty and absent dates; midnight intervals; overlaps; and duplicate
#' uploads. All identities, app names and responses were invented for teaching.
#' Dates run from 1 to 7 October 2025. Integer-like timestamps and durations in
#' `*_ms` columns are numeric milliseconds; datetime columns are `POSIXct` and
#' calendar dates are `Date`. Unavailable grains are typed zero-row tables.
#'
#' Use one record at a time. Summing all records would combine different sources,
#' including two uploads for one identity. Likewise, event/episode/daily are
#' different views of use, and `ALL` totals in day exports are not extra apps.
#' Mixed default episodes and daily totals come from line, with events from meta
#' Table 2. App exports describe the recorded application coverage.
#'
#' The object is rebuilt by this installed script:
#' \preformatted{examples/rebuild-appusage-demo.R}
#' It uses [run_first_level_appusage()] and [run_second_level_appusage()] with defaults
#' and the stated timezone. The original TXT/CSV/XLSX files and golden expectations
#' are under `system.file("extdata", "synth", package = "appusageR")`.
#' See [appusage_synthetic_project] for the full scenario guide and project demo.
#'
#' @source Deterministically constructed synthetic data, distributed under GPL-3.
#' @seealso [assess_appusage_qc()], [qc_appusage_day()],
#'   [audit_appusage_days()], [match_appusage_self_report()]
#' @examples
#' data(appusage_demo)
#' names(appusage_demo$records)
#' mixed <- appusage_demo$records$mixed_line_meta
#' vapply(mixed$data, nrow, integer(1))
#' stopifnot(nrow(mixed$data$event) == 28L,
#'           sum(mixed$data$daily$duration_ms) == 12600000)
#' qc <- assess_appusage_qc(mixed, metadata = mixed$metadata,
#'                         participant_id = "105")
#' qc$qc[c("pass_qc", "analysis_eligible_daily")]
"appusage_demo"
