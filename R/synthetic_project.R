#' Synthetic project exports and a linked fictional questionnaire
#'
#' Fifteen deterministic fictional uploads illustrate four pure export formats,
#' mixed line/meta content, coverage gaps, midnight intervals, overlaps,
#' duplicate uploads and diagnosed input failures. A 17-row questionnaire
#' illustrates successful and unsuccessful two-key matching.
#'
#' @details
#' Locate the files with `system.file("extdata", "synth",
#' package = "appusageR")`. The project has the character ProjectID `"009"`.
#' `scenarios.csv` describes each source and its independently specified expected
#' counts, duration and default day-QC result. Durations exclude the `ALL` total
#' rows retained in day exports. `expected-links.csv` lists the expected status
#' of every questionnaire row and the selected source for successful matches.
#'
#' `ProjectID-009-WJXraw-synthetic.xlsx` contains `Survey` and `Fields` sheets;
#' `survey.csv` is its text equivalent. Use `sequence_id`, `upload` and
#' `submitted_at` as the matching columns. Dates represent local wall time in
#' Asia/Shanghai. The blank sleep response and zero self-reported phone use are
#' intentional. Every value and identity was created for teaching, under GPL-3.
#'
#' Run `demo("synthetic-project", package = "appusageR", ask = FALSE)` for
#' processing with two file workers, QC and questionnaire matching. It writes
#' only to a temporary output directory and retains the five input diagnoses.
#' Project files are processed in parallel; this does not run multiple projects
#' simultaneously. See the installed `README.md` beside the data for the full
#' scenario map and expected totals.
#'
#' @name appusage_synthetic_project
#' @seealso [run_appusage_project_workflow()], [read_appusage_batch()],
#'   [match_appusage_self_report()], [audit_appusage_days()], [appusage_demo]
#' @examples
#' folder <- system.file("extdata", "synth", package = "appusageR",
#'                       mustWork = TRUE)
#' scenarios <- read.csv(file.path(folder, "scenarios.csv"))
#' table(scenarios$expected_status)
#' survey <- read.csv(file.path(folder, "survey.csv"), colClasses = "character")
#' stopifnot(nrow(survey) == 17L)
NULL
