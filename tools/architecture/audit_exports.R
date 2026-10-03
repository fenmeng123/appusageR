# Compare the delivered questionnaire workbook with its authoritative LinkResult.
# Usage: Rscript audit_exports.R /private/project /private/audit
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
library(appusageR)
project <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
destination <- args[[2]]
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
adapter <- readRDS(file.path(project, "workflow_configuration.rds"))
links <- readRDS(file.path(project, "self_report_link_result.rds"))
expected <- links$matched_self_report
path <- get("appusage_matched_excel_path", asNamespace("appusageR"))(
  self_report_file = adapter$resolved_self_report_file,
  project_root = project, project_id = adapter$project_id)
actual <- readxl::read_excel(path, col_types = "text", .name_repair = "minimal")
stopifnot(identical(names(actual), names(expected)), nrow(actual) == nrow(expected))
# Excel blank cells cannot distinguish an empty string from NA. No scientific
# RDA or JSON comparison uses this workbook-only representation normalization.
cell_text <- function(x) {
  out <- as.character(x)
  out[!is.na(out) & out == ""] <- NA_character_
  out
}
equal <- vapply(names(expected), function(name)
  identical(cell_text(actual[[name]]), cell_text(expected[[name]])), logical(1))
write.csv(data.frame(column = names(expected), identical_cells = unname(equal)),
  file.path(destination, "workbook_column_audit.csv"), row.names = FALSE)
if (!all(equal)) stop("Workbook cell values differ from LinkResult; see private column audit")
matched <- actual$moSens_match_status == "matched"
stopifnot(!anyNA(matched),
  all(file.exists(file.path(project, actual$moSens_data_dir[matched]))))
jsonlite::write_json(list(rows = nrow(actual), columns = ncol(actual),
  cells_compared = nrow(actual) * ncol(actual), matched = sum(matched),
  unmatched = sum(!matched), all_matched_paths_exist = TRUE,
  status = "passed"), file.path(destination, "workbook_audit.json"),
  pretty = TRUE, auto_unbox = TRUE)
cat("All exported workbook cells agree with LinkResult.\n")
