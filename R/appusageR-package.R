#' Process Android usage exports: a task-based guide
#'
#' Parse APP Usage / Screen Time exports, create standardized research tables,
#' assess quality, append application categories and link questionnaire rows.
#' Start with the shared fictional project and choose a task below.
#'
#' @section Six runnable tutorials:
#' \enumerate{
#'   \item \code{vignette("appusageR-workflow", package = "appusageR")}:
#'     read two uploads, run the workflow and QC, and inspect paired caches.
#'   \item \code{vignette("formats-and-data-model", package = "appusageR")}:
#'     compare four pure formats, mixed content and explicit/legacy selection.
#'   \item \code{vignette("day-and-overlap-audits", package = "appusageR")}:
#'     explain QC denominators, blank days, overlap and midnight splitting.
#'   \item \code{vignette("caches-and-recovery", package = "appusageR")}:
#'     reuse completed stages, diagnose a partial input and repair temporary caches.
#'   \item \code{vignette("categories-and-dictionary", package = "appusageR")}:
#'     resolve category candidates, profile unknowns and update annotations.
#'   \item \code{vignette("projects-and-surveys", package = "appusageR")}:
#'     run two projects, resume all-failure delivery and retain every survey row.
#' }
#' Each tutorial uses installed synthetic inputs, temporary output and one worker.
#' [appusage_synthetic_project] also describes a runnable two-worker project demo.
#'
#' @section Choose an interface:
#' For project execution use [appusage_config()], [run_appusage_workflow()] and
#' [run_appusage_projects()]. [run_appusage_project_workflow()] adds the
#' Wenjuanxing folder/workbook adapter. [appusage_configuration] owns the full
#' parameter reference; [appusage_demo] owns the teaching data description.
#'
#' For one source use [run_first_level_appusage()] and
#' [run_second_level_appusage()]. Lower-level [parse_line()], [parse_meta()],
#' [parse_day()] and [parse_app()] expose native structures.
#' [assess_appusage_qc()] evaluates complete QC; [run_qc_appusage()] in-memory
#' compatibility mode evaluates coverage. [audit_appusage_days()] and
#' [audit_appusage_overlap()] return explicit diagnostic views.
#'
#' For existing caches use [plan_appusage_workflow()], [run_appusage_stage()],
#' [update_app_categories_cache()] and [validate_appusage_project()]. For explicit
#' questionnaire relationships use [match_appusage_self_report()].
#'
#' @section Read the outputs:
#' Each source has proc-1 and proc-2 RDA/JSON pairs. Every RDA owns one object
#' named `data`; proc-2 contains typed event, episode and daily tables. Routine
#' QC and category metadata belong to proc-2 JSON. Execution success, QC pass,
#' grain eligibility and complete project delivery have separate denominators.
#' Package, scientific schema and manual dictionary versions are independent.
#'
#' @seealso [app_category_dictionary], [appusage_configuration],
#'   [appusage_demo], [appusage_synthetic_project]
#' @examples
#' data(appusage_demo)
#' names(appusage_demo$records)
#' record <- appusage_demo$records$line_seven_days
#' vapply(record$data, nrow, integer(1))
#' # Open the installed task index with help("appusageR").
#' @name appusageR
#' @aliases appusageR-package
"_PACKAGE"
