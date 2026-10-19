appusage_category_transaction_paths <- function(rda, json) {
  list(temp_rda = paste0(rda, ".appcategories-tmp"), temp_json = paste0(json, ".appcategories-tmp"),
    backup_rda = paste0(rda, ".appcategories-bak"), backup_json = paste0(json, ".appcategories-bak"),
    journal = paste0(json, ".appcategories-journal.rds"))
}

# Parsed JSON arrays are lists of scalar elements. Compact homogeneous arrays
# for serialization, retaining array shape even at length one and keeping nulls.
appusage_category_json_ready <- function(value) {
  if (!is.list(value) || is.data.frame(value)) return(value)
  if (is.null(names(value)) && length(value) &&
      all(vapply(value, function(x) is.atomic(x) && length(x) == 1L &&
        !is.object(x) && !is.na(x), logical(1)))) {
    types <- vapply(value, typeof, character(1))
    same <- length(unique(types)) == 1L || all(types %in% c("integer", "double"))
    if (same && all(types %in% c("logical", "integer", "double", "character")))
      return(I(unlist(value, use.names = FALSE)))
  }
  lapply(value, appusage_category_json_ready)
}

appusage_category_recover_pair <- function(rda, json) {
  paths <- appusage_category_transaction_paths(rda, json)
  if (file.exists(paths$journal)) {
    record <- readRDS(paths$journal)
    if (!identical(record$rda, rda) || !identical(record$json, json))
      cli::cli_abort("Category recovery journal targets do not match the pair.")
    metadata <- appusage_read_json_safely(json)
    committed <- file.exists(rda) && file.exists(json) &&
      appusage_artifact_valid(rda, record$new_artifact) &&
      appusage_contract_equal(metadata$module_state$category, record$new_contract) &&
      appusage_contract_equal(metadata$module_state$artifact, record$new_artifact)
    if (!committed) {
      if (file.exists(paths$backup_rda)) {
        if (!appusage_artifact_valid(paths$backup_rda, record$old_artifact))
          cli::cli_abort("Category RDA recovery backup failed integrity validation.")
        if (file.exists(rda)) unlink(rda)
        if (!file.rename(paths$backup_rda, rda)) cli::cli_abort("Could not restore category RDA backup.")
      }
      if (file.exists(paths$backup_json)) {
        if (!identical(unname(appusage_file_md5(paths$backup_json)), record$old_json_md5))
          cli::cli_abort("Category JSON recovery backup failed integrity validation.")
        if (file.exists(json)) unlink(json)
        if (!file.rename(paths$backup_json, json)) cli::cli_abort("Could not restore category JSON backup.")
      }
      if (!file.exists(rda) || !file.exists(json) || !appusage_artifact_valid(rda, record$old_artifact) ||
          !identical(unname(appusage_file_md5(json)), record$old_json_md5))
        cli::cli_abort("Interrupted category pair cannot be recovered from its journal; evidence retained.")
    }
    appusage_cleanup_paths(unlist(paths, use.names = FALSE))
  } else {
    if (file.exists(paths$backup_rda) || file.exists(paths$backup_json))
      cli::cli_abort("Category backups lack a recovery journal; evidence retained.")
    appusage_cleanup_paths(c(paths$temp_rda, paths$temp_json))
  }
  invisible(NULL)
}
