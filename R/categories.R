#' Read a manual app category dictionary
#'
#' Reads and standardizes the user's manually coded app category dictionary.
#' The dictionary must contain `App_UUID`, `App_Name`, `Level_1_Category`, and
#' `Level_2_Category`. `App_UUID` is matched against APP Usage `package_name`.
#'
#' @param path Path to an Excel dictionary file.
#' @param sheet Sheet passed to [readxl::read_excel()].
#' @param ... Additional arguments passed to [readxl::read_excel()].
#'
#' @return A tibble with dictionary columns and matching diagnostics as
#'   attributes.
#' @export
read_app_category_dictionary <- function(path, sheet = 1, ...) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg readxl} is required to read app category dictionaries."
    ))
  }
  if (length(path) != 1 || is.na(path) || !file.exists(path)) {
    cli::cli_abort("Dictionary file does not exist: {.path {path}}")
  }
  raw <- readxl::read_excel(path, sheet = sheet, ...)
  standardize_category_dict(
    raw,
    dictionary_source = normalizePath(path, winslash = "/", mustWork = FALSE),
    dictionary_sheet = as.character(sheet)
  )
}

#' Add manual app categories to APP Usage data
#'
#' Adds `Level_1_Category` and `Level_2_Category` using a manually coded
#' dictionary. Matching first uses `package_name` against dictionary `App_UUID`,
#' then falls back to unambiguous exact app-name matches against
#' `App_Name_Repaired` and `App_Name`.
#'
#' @param data A data frame or second-level APP Usage `data` list.
#' @param dictionary A dictionary data frame, a standardized dictionary returned
#'   by `read_app_category_dictionary()`, or a dictionary file path.
#' @param overwrite Whether to overwrite existing non-missing category values.
#'
#' @return `data` with category columns added. Matching summaries are attached
#'   as the `app_category_summary` attribute.
#' @export
add_app_categories <- function(data, dictionary, overwrite = TRUE) {
  dictionary <- as_app_category_dictionary(dictionary)

  if (is.data.frame(data)) {
    return(add_app_categories_frame(data, dictionary, overwrite = overwrite))
  }
  if (!is.list(data)) {
    cli::cli_abort(c(
      "`data` must be a data frame or a second-level APP Usage list."
    ))
  }

  out <- data
  summaries <- list()
  for (nm in names(out)) {
    has_app_id <- any(c("package_name", "app_name") %in% names(out[[nm]]))
    if (is.data.frame(out[[nm]]) && has_app_id) {
      out[[nm]] <- add_app_categories_frame(
        out[[nm]],
        dictionary,
        overwrite = overwrite
      )
      summary <- attr(out[[nm]], "app_category_summary", exact = TRUE)
      summary$grain <- nm
      summaries[[length(summaries) + 1L]] <- summary
    }
  }
  attr(out, "app_category_summary") <- combine_category_summaries(summaries)
  out
}

#' Batch add manual app categories to second-level caches
#'
#' Reads each `proc-2` RDA in a project, adds manual app categories to every
#' second-level table with app identifiers, and writes the updated `data` object
#' back to the same `proc-2` RDA file.
#'
#' @param project_dir Project folder produced by [read_appusage_batch()] and
#'   [write_second_level_batch()].
#' @param dictionary Dictionary data frame, standardized dictionary, or file
#'   path readable by `read_app_category_dictionary()`.
#' @param overwrite Whether to overwrite existing non-missing category values.
#' @param progress Whether to print simple progress messages.
#'
#' @return Invisibly returns a tibble summary of category matching.
#' @export
write_app_categories_batch <- function(project_dir, dictionary,
                                       overwrite = TRUE,
                                       progress = TRUE) {
  if (
    length(project_dir) != 1 ||
      is.na(project_dir) ||
      !dir.exists(project_dir)
  ) {
    cli::cli_abort("`project_dir` must be an existing project directory.")
  }
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  proclevel_2 <- file.path(project_dir, "proclevel-2")
  if (!dir.exists(proclevel_2)) {
    cli::cli_abort("Project is missing {.path proclevel-2}.")
  }
  dictionary <- as_app_category_dictionary(dictionary)
  second_level_files <- sort(list.files(
    proclevel_2,
    pattern = "_proc-2[.]rda$",
    full.names = TRUE
  ))
  if (length(second_level_files) == 0) {
    cli::cli_abort(c(
      "No second-level RDA files were found in {.path {proclevel_2}}."
    ))
  }

  rows <- vector("list", length(second_level_files))
  for (i in seq_along(second_level_files)) {
    if (isTRUE(progress) && (i == 1 || i == length(second_level_files))) {
      message(sprintf(
        "Adding app categories to second-level file %d/%d",
        i,
        length(second_level_files)
      ))
    }
    rows[[i]] <- write_app_categories_one(
      second_level_rda = second_level_files[[i]],
      project_dir = project_dir,
      dictionary = dictionary,
      overwrite = overwrite
    )
  }
  summary <- tibble::as_tibble(do.call(rbind, rows))
  update_category_qc_summary(project_dir)
  invisible(summary)
}

standardize_category_dict <- function(dictionary,
                                      dictionary_source = NA_character_,
                                      dictionary_sheet = NA_character_) {
  dictionary <- tibble::as_tibble(dictionary)
  required <- c("App_UUID", "App_Name", "Level_1_Category", "Level_2_Category")
  missing <- setdiff(required, names(dictionary))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Dictionary is missing required column(s): ",
      paste(missing, collapse = ", ")
    ))
  }
  if (!"App_Name_Repaired" %in% names(dictionary)) {
    dictionary$App_Name_Repaired <- NA_character_
  }
  dictionary <- dictionary[, c(
    "App_Name", "App_Name_Repaired", "App_UUID",
    "Level_1_Category", "Level_2_Category"
  )]
  dictionary$App_Name <- category_trim(dictionary$App_Name)
  dictionary$App_Name_Repaired <- category_trim(dictionary$App_Name_Repaired)
  dictionary$App_UUID <- category_trim(dictionary$App_UUID)
  dictionary$Level_1_Category <- category_trim(dictionary$Level_1_Category)
  dictionary$Level_2_Category <- category_trim(dictionary$Level_2_Category)
  dictionary$.app_uuid_key <- standardize_package_name(dictionary$App_UUID)
  dictionary$.app_name_key <- category_name_key(dictionary$App_Name)
  dictionary$.app_name_repaired_key <- category_name_key(
    dictionary$App_Name_Repaired
  )

  diagnostics <- dictionary_diagnostics(dictionary)
  attr(dictionary, "dictionary_source") <- dictionary_source
  attr(dictionary, "dictionary_sheet") <- dictionary_sheet
  attr(dictionary, "app_category_lookups") <- build_category_lookups(dictionary)
  attr(dictionary, "app_category_diagnostics") <- diagnostics
  class(dictionary) <- unique(c(
    "appusage_category_dictionary",
    class(dictionary)
  ))
  dictionary
}

as_app_category_dictionary <- function(dictionary) {
  if (inherits(dictionary, "appusage_category_dictionary")) {
    return(dictionary)
  }
  if (is.character(dictionary) && length(dictionary) == 1) {
    return(read_app_category_dictionary(dictionary))
  }
  if (is.data.frame(dictionary)) {
    return(standardize_category_dict(dictionary))
  }
  cli::cli_abort("`dictionary` must be a file path or data frame.")
}

build_category_lookups <- function(dictionary) {
  list(
    app_uuid = build_category_lookup(dictionary, ".app_uuid_key"),
    app_name_repaired = build_category_lookup(
      dictionary,
      ".app_name_repaired_key"
    ),
    app_name = build_category_lookup(dictionary, ".app_name_key")
  )
}

build_category_lookup <- function(dictionary, key_col) {
  key <- dictionary[[key_col]]
  keep <- !is.na(key) &
    !is.na(dictionary$Level_1_Category) &
    !is.na(dictionary$Level_2_Category)
  if (!any(keep)) {
    return(list(
      matches = empty_category_lookup(),
      conflicts = empty_category_conflicts()
    ))
  }
  working <- dictionary[
    keep,
    c(key_col, "Level_1_Category", "Level_2_Category"),
    drop = FALSE
  ]
  names(working)[[1]] <- "key"
  groups <- split(working, working$key)
  matched <- list()
  conflicts <- list()
  for (key_value in names(groups)) {
    values <- unique(groups[[key_value]][
      ,
      c("Level_1_Category", "Level_2_Category"),
      drop = FALSE
    ])
    if (nrow(values) == 1) {
      matched[[length(matched) + 1L]] <- data.frame(
        key = key_value,
        Level_1_Category = values$Level_1_Category[[1]],
        Level_2_Category = values$Level_2_Category[[1]],
        stringsAsFactors = FALSE
      )
    } else {
      conflicts[[length(conflicts) + 1L]] <- data.frame(
        key = key_value,
        n_category_pairs = nrow(values),
        categories = paste(
          paste(values$Level_1_Category, values$Level_2_Category, sep = " / "),
          collapse = " | "
        ),
        stringsAsFactors = FALSE
      )
    }
  }
  list(
    matches = bind_data_frames(matched, empty_category_lookup()),
    conflicts = bind_data_frames(conflicts, empty_category_conflicts())
  )
}

add_app_categories_frame <- function(data, dictionary, overwrite) {
  lookups <- attr(dictionary, "app_category_lookups", exact = TRUE)
  out <- data
  n <- nrow(out)
  if (!"Level_1_Category" %in% names(out)) out$Level_1_Category <- NA_character_
  if (!"Level_2_Category" %in% names(out)) out$Level_2_Category <- NA_character_
  out$app_category_match_method <- NA_character_
  out$app_category_match_key <- NA_character_
  out$app_category_match_status <- "unmatched"

  needs <- rep(TRUE, n)
  if (!isTRUE(overwrite)) {
    needs <- is.na(category_trim(out$Level_1_Category)) |
      is.na(category_trim(out$Level_2_Category))
    out$app_category_match_status[!needs] <- "preexisting"
  } else {
    out$Level_1_Category <- NA_character_
    out$Level_2_Category <- NA_character_
  }

  package_key <- if ("package_name" %in% names(out)) {
    standardize_package_name(out$package_name)
  } else {
    rep(NA_character_, n)
  }
  app_name_key <- if ("app_name" %in% names(out)) {
    category_name_key(out$app_name)
  } else {
    rep(NA_character_, n)
  }

  out <- apply_category_match(
    out,
    row_key = package_key,
    lookup = lookups$app_uuid,
    method = "app_uuid",
    eligible = needs
  )
  no_uuid_conflict <- out$app_category_match_status != "conflict"
  out <- apply_category_match(
    out,
    row_key = app_name_key,
    lookup = lookups$app_name_repaired,
    method = "app_name_repaired",
    eligible = needs &
      no_uuid_conflict &
      out$app_category_match_status == "unmatched"
  )
  no_name_repaired_conflict <- out$app_category_match_status != "conflict"
  out <- apply_category_match(
    out,
    row_key = app_name_key,
    lookup = lookups$app_name,
    method = "app_name",
    eligible = needs & no_uuid_conflict & no_name_repaired_conflict &
      out$app_category_match_status == "unmatched"
  )

  attr(out, "app_category_summary") <- summarize_category_frame(
    out,
    package_key,
    app_name_key
  )
  out
}

apply_category_match <- function(data, row_key, lookup, method, eligible) {
  key <- row_key
  key[!eligible] <- NA_character_
  conflict <- !is.na(key) & key %in% lookup$conflicts$key
  data$app_category_match_status[conflict] <- "conflict"
  data$app_category_match_method[conflict] <- paste0(method, "_conflict")
  data$app_category_match_key[conflict] <- key[conflict]

  idx <- match(key, lookup$matches$key)
  matched <- !is.na(idx) & !conflict
  data$Level_1_Category[matched] <-
    lookup$matches$Level_1_Category[idx[matched]]
  data$Level_2_Category[matched] <-
    lookup$matches$Level_2_Category[idx[matched]]
  data$app_category_match_status[matched] <- "matched"
  data$app_category_match_method[matched] <- method
  data$app_category_match_key[matched] <- key[matched]
  data
}

summarize_category_frame <- function(data, package_key, app_name_key) {
  app_key <- ifelse(
    !is.na(package_key),
    paste0("pkg:", package_key),
    paste0("name:", app_name_key)
  )
  app_key[is.na(package_key) & is.na(app_name_key)] <- NA_character_
  matched <- data$app_category_match_status == "matched"
  conflict <- data$app_category_match_status == "conflict"
  unique_apps <- unique(stats::na.omit(app_key))
  matched_apps <- unique(stats::na.omit(app_key[matched]))
  method_counts <- table(factor(
    data$app_category_match_method[matched],
    levels = c("app_uuid", "app_name_repaired", "app_name")
  ))
  list(
    n_category_rows = nrow(data),
    n_category_matched_rows = sum(matched, na.rm = TRUE),
    n_category_unmatched_rows = sum(
      data$app_category_match_status == "unmatched",
      na.rm = TRUE
    ),
    n_category_conflict_rows = sum(conflict, na.rm = TRUE),
    category_row_match_rate = safe_rate(sum(matched, na.rm = TRUE), nrow(data)),
    n_category_unique_apps = length(unique_apps),
    n_category_matched_apps = length(matched_apps),
    n_category_unmatched_apps = max(
      length(unique_apps) - length(matched_apps),
      0L
    ),
    category_match_rate = safe_rate(length(matched_apps), length(unique_apps)),
    n_category_app_uuid_rows = unname(method_counts[["app_uuid"]]),
    n_category_app_name_repaired_rows =
      unname(method_counts[["app_name_repaired"]]),
    n_category_app_name_rows = unname(method_counts[["app_name"]])
  )
}

combine_category_summaries <- function(summaries) {
  if (length(summaries) == 0) {
    return(empty_category_summary())
  }
  rows <- lapply(summaries, function(x) {
    x$grain <- x$grain %||% NA_character_
    as.data.frame(x, stringsAsFactors = FALSE)
  })
  frame <- tibble::as_tibble(do.call(rbind, rows))
  method_cols <- c(
    "n_category_app_uuid_rows",
    "n_category_app_name_repaired_rows",
    "n_category_app_name_rows"
  )
  list(
    by_grain = frame,
    n_category_rows = sum(frame$n_category_rows, na.rm = TRUE),
    n_category_matched_rows = sum(frame$n_category_matched_rows, na.rm = TRUE),
    n_category_unmatched_rows = sum(
      frame$n_category_unmatched_rows,
      na.rm = TRUE
    ),
    n_category_conflict_rows = sum(
      frame$n_category_conflict_rows,
      na.rm = TRUE
    ),
    category_row_match_rate = safe_rate(
      sum(frame$n_category_matched_rows, na.rm = TRUE),
      sum(frame$n_category_rows, na.rm = TRUE)
    ),
    n_category_unique_apps = sum(frame$n_category_unique_apps, na.rm = TRUE),
    n_category_matched_apps = sum(frame$n_category_matched_apps, na.rm = TRUE),
    n_category_unmatched_apps = sum(
      frame$n_category_unmatched_apps,
      na.rm = TRUE
    ),
    category_match_rate = safe_rate(
      sum(frame$n_category_matched_apps, na.rm = TRUE),
      sum(frame$n_category_unique_apps, na.rm = TRUE)
    ),
    n_category_app_uuid_rows = sum(frame[[method_cols[[1]]]], na.rm = TRUE),
    n_category_app_name_repaired_rows = sum(
      frame[[method_cols[[2]]]],
      na.rm = TRUE
    ),
    n_category_app_name_rows = sum(frame[[method_cols[[3]]]], na.rm = TRUE)
  )
}

write_app_categories_one <- function(second_level_rda, project_dir,
                                     dictionary, overwrite) {
  started_at <- Sys.time()
  entities <- parse_appusage_filename(second_level_rda)
  result <- tryCatch(
    {
      data <- load_appusage_data_object(second_level_rda)
      data <- add_app_categories(data, dictionary, overwrite = overwrite)
      summary <- attr(data, "app_category_summary", exact = TRUE)
      save(data, file = second_level_rda)
      update_matching_second_level_metadata(
        project_dir,
        second_level_rda,
        summary,
        dictionary
      )
      list(status = "success", error = NULL, summary = summary)
    },
    error = function(e) {
      list(status = "error", error = e, summary = empty_category_summary())
    }
  )
  finished_at <- Sys.time()
  category_summary_row(
    entities = entities,
    second_level_rda = second_level_rda,
    status = result$status,
    summary = result$summary,
    error = result$error,
    started_at = started_at,
    finished_at = finished_at
  )
}

update_matching_second_level_metadata <- function(project_dir, second_level_rda,
                                                 summary, dictionary) {
  entities <- parse_appusage_filename(second_level_rda)
  metadata_file <- file.path(
    project_dir,
    "proclevel-2",
    build_appusage_filename(
      participant_id = entities$sub %||% "unknown",
      export_type = entities$type %||% "unknown",
      proc = 2,
      extension = "json"
    )
  )
  if (!file.exists(metadata_file)) {
    metadata_file <- create_missing_second_level_metadata(second_level_rda, project_dir)
  }
  metadata <- jsonlite::read_json(metadata_file, simplifyVector = TRUE)
  metadata <- update_category_metadata(
    metadata,
    summary,
    dictionary,
    second_level_rda
  )
  write_metadata_json(metadata, metadata_file)
  invisible(metadata_file)
}

update_category_metadata <- function(metadata, summary, dictionary,
                                     second_level_rda) {
  if (is.null(metadata$processing) || !is.list(metadata$processing)) {
    metadata$processing <- list()
  }
  if (is.null(metadata$counts) || !is.list(metadata$counts)) {
    metadata$counts <- list()
  }
  if (is.null(metadata$outputs) || !is.list(metadata$outputs)) {
    metadata$outputs <- list()
  }
  diagnostics <- attr(dictionary, "app_category_diagnostics", exact = TRUE)
  metadata$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  metadata$processing$app_category_status <- "success"
  metadata$outputs$second_level_rda <- normalizePath(
    second_level_rda,
    winslash = "/",
    mustWork = FALSE
  )
  metadata$counts$n_category_matched_apps <- summary$n_category_matched_apps
  metadata$counts$category_match_rate <- summary$category_match_rate
  metadata$category_dictionary <- c(
    dictionary_metadata(dictionary),
    list(
      n_category_rows = summary$n_category_rows,
      n_category_matched_rows = summary$n_category_matched_rows,
      n_category_unmatched_rows = summary$n_category_unmatched_rows,
      n_category_conflict_rows = summary$n_category_conflict_rows,
      category_row_match_rate = summary$category_row_match_rate,
      n_category_unique_apps = summary$n_category_unique_apps,
      n_category_matched_apps = summary$n_category_matched_apps,
      n_category_unmatched_apps = summary$n_category_unmatched_apps,
      category_match_rate = summary$category_match_rate,
      n_category_app_uuid_rows = summary$n_category_app_uuid_rows,
      n_category_app_name_repaired_rows =
        summary$n_category_app_name_repaired_rows,
      n_category_app_name_rows = summary$n_category_app_name_rows,
      dictionary_n_app_uuid_conflicts = diagnostics$n_app_uuid_conflicts,
      dictionary_n_app_name_repaired_conflicts =
        diagnostics$n_app_name_repaired_conflicts,
      dictionary_n_app_name_conflicts = diagnostics$n_app_name_conflicts
    )
  )
  metadata
}

update_category_qc_summary <- function(project_dir) {
  proclevel_2 <- file.path(project_dir, "proclevel-2")
  if (!dir.exists(proclevel_2)) {
    return(invisible(NULL))
  }
  metadata_files <- sort(list.files(
    proclevel_2,
    pattern = "_proc-2[.]json$",
    full.names = TRUE
  ))
  if (length(metadata_files) == 0) {
    return(invisible(NULL))
  }
  summary <- build_qc_summary_from_metadata(metadata_files)
  summary_file <- file.path(
    project_dir,
    "analytic_summary_table_proclevel-2.csv"
  )
  utils::write.csv(summary, summary_file, row.names = FALSE, na = "")
  write_dataset_description_json(
    project_info_from_root(project_dir),
    summary = summary,
    proclevel = 2,
    summary_file = summary_file,
    status = "success"
  )
  invisible(summary)
}

category_summary_row <- function(entities, second_level_rda, status, summary,
                                 error, started_at, finished_at) {
  data.frame(
    participant_id = entities$sub %||% NA_character_,
    detected_type = entities$type %||% NA_character_,
    status = status,
    second_level_rda = normalizePath(
      second_level_rda,
      winslash = "/",
      mustWork = FALSE
    ),
    n_category_rows = summary$n_category_rows,
    n_category_matched_rows = summary$n_category_matched_rows,
    n_category_unmatched_rows = summary$n_category_unmatched_rows,
    n_category_conflict_rows = summary$n_category_conflict_rows,
    category_row_match_rate = summary$category_row_match_rate,
    n_category_unique_apps = summary$n_category_unique_apps,
    n_category_matched_apps = summary$n_category_matched_apps,
    n_category_unmatched_apps = summary$n_category_unmatched_apps,
    category_match_rate = summary$category_match_rate,
    n_category_app_uuid_rows = summary$n_category_app_uuid_rows,
    n_category_app_name_repaired_rows =
      summary$n_category_app_name_repaired_rows,
    n_category_app_name_rows = summary$n_category_app_name_rows,
    error_message = if (is.null(error)) {
      NA_character_
    } else {
      conditionMessage(error)
    },
    started_at = format(started_at, "%Y-%m-%d %H:%M:%OS3 %z"),
    finished_at = format(finished_at, "%Y-%m-%d %H:%M:%OS3 %z"),
    elapsed_sec = as.numeric(difftime(finished_at, started_at, units = "secs")),
    stringsAsFactors = FALSE
  )
}

dictionary_metadata <- function(dictionary) {
  diagnostics <- attr(dictionary, "app_category_diagnostics", exact = TRUE)
  list(
    dictionary_source =
      attr(dictionary, "dictionary_source", exact = TRUE) %||% NA_character_,
    dictionary_sheet =
      attr(dictionary, "dictionary_sheet", exact = TRUE) %||% NA_character_,
    dictionary_n_rows = diagnostics$n_rows,
    dictionary_n_unique_app_uuid = diagnostics$n_unique_app_uuid,
    dictionary_n_unique_app_name = diagnostics$n_unique_app_name,
    dictionary_n_unique_app_name_repaired =
      diagnostics$n_unique_app_name_repaired
  )
}

dictionary_diagnostics <- function(dictionary) {
  lookups <- build_category_lookups(dictionary)
  list(
    n_rows = nrow(dictionary),
    n_unique_app_uuid = length(
      unique(stats::na.omit(dictionary$.app_uuid_key))
    ),
    n_unique_app_name = length(
      unique(stats::na.omit(dictionary$.app_name_key))
    ),
    n_unique_app_name_repaired = length(
      unique(stats::na.omit(dictionary$.app_name_repaired_key))
    ),
    n_app_uuid_conflicts = nrow(lookups$app_uuid$conflicts),
    n_app_name_repaired_conflicts = nrow(lookups$app_name_repaired$conflicts),
    n_app_name_conflicts = nrow(lookups$app_name$conflicts)
  )
}

empty_category_summary <- function() {
  list(
    n_category_rows = NA_integer_,
    n_category_matched_rows = NA_integer_,
    n_category_unmatched_rows = NA_integer_,
    n_category_conflict_rows = NA_integer_,
    category_row_match_rate = NA_real_,
    n_category_unique_apps = NA_integer_,
    n_category_matched_apps = NA_integer_,
    n_category_unmatched_apps = NA_integer_,
    category_match_rate = NA_real_,
    n_category_app_uuid_rows = NA_integer_,
    n_category_app_name_repaired_rows = NA_integer_,
    n_category_app_name_rows = NA_integer_
  )
}

empty_category_lookup <- function() {
  data.frame(
    key = character(),
    Level_1_Category = character(),
    Level_2_Category = character(),
    stringsAsFactors = FALSE
  )
}

empty_category_conflicts <- function() {
  data.frame(
    key = character(),
    n_category_pairs = integer(),
    categories = character(),
    stringsAsFactors = FALSE
  )
}

bind_data_frames <- function(rows, empty) {
  if (length(rows) == 0) {
    return(empty)
  }
  do.call(rbind, rows)
}

category_trim <- function(x) {
  x <- stringr::str_trim(as.character(x))
  x[x %in% c("", "NA", "NULL", "null", "NaN")] <- NA_character_
  x
}

category_name_key <- function(x) {
  category_trim(x)
}

safe_rate <- function(numerator, denominator) {
  if (is.na(denominator) || denominator <= 0) {
    return(NA_real_)
  }
  numerator / denominator
}
