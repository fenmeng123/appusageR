#' Quality-control day-level APP Usage records
#'
#' Computes participant-level mobile-sensing QC flags without deleting rows from
#' the input data. By default, daily totals are calculated from app rows. If an
#' APP Usage `ALL` row appears to be an app-generated daily total, it is excluded
#' from the calculation set to avoid double counting.
#'
#' @param data Day-level APP Usage data from [parse_day()] or [parse_app()].
#' @param participant_col Participant ID column.
#' @param date_col Date column.
#' @param duration_col Duration column in milliseconds.
#' @param require_all_weekdays Whether Monday through Sunday must be covered.
#' @param min_nonempty_days Minimum number of non-empty recorded days.
#' @param use_all_apps_row If `TRUE`, use the APP Usage ALL row whenever
#'   available. The default is `FALSE` so totals are consistently calculated
#'   from app rows.
#' @param include_collection_app Whether `com.w.appusage` contributes to daily
#'   totals.
#' @param drop_likely_total_all_rows Whether likely total ALL rows should be
#'   excluded when app-row totals are used.
#' @param all_row_tolerance Relative tolerance used when judging whether an ALL
#'   row looks like a generated daily total.
#'
#' @return One row per participant with QC metrics and flags.
#' @export
qc_appusage_day <- function(data, participant_col = "participant_id",
                            date_col = "date",
                            duration_col = "duration_ms",
                            require_all_weekdays = TRUE,
                            min_nonempty_days = 7,
                            use_all_apps_row = FALSE,
                            include_collection_app = TRUE,
                            drop_likely_total_all_rows = TRUE,
                            all_row_tolerance = 0.10) {
  required <- c(participant_col, date_col, duration_col)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing required columns: {.field {missing_cols}}")
  }

  prepared <- standardize_qc_input(data, participant_col, date_col, duration_col)
  participants <- unique(prepared$participant_id)
  rows <- lapply(participants, function(id) {
    participant_data <- prepared[prepared$participant_id == id, , drop = FALSE]
    daily <- appusage_daily_totals(
      participant_data,
      use_all_apps_row = use_all_apps_row,
      include_collection_app = include_collection_app,
      drop_likely_total_all_rows = drop_likely_total_all_rows,
      all_row_tolerance = all_row_tolerance
    )
    nonempty <- daily[!is.na(daily$date) & daily$total_duration_ms > 0, , drop = FALSE]
    weekdays <- unique(weekday_name(nonempty$date))
    has <- weekday_flags(weekdays)

    data.frame(
      participant_id = id,
      n_recorded_days = length(unique(stats::na.omit(participant_data$date))),
      n_nonempty_days = length(unique(stats::na.omit(nonempty$date))),
      weekdays_covered = paste(sort_weekdays(weekdays), collapse = ", "),
      has_monday = has[["Monday"]],
      has_tuesday = has[["Tuesday"]],
      has_wednesday = has[["Wednesday"]],
      has_thursday = has[["Thursday"]],
      has_friday = has[["Friday"]],
      has_saturday = has[["Saturday"]],
      has_sunday = has[["Sunday"]],
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out$pass_min_days <- out$n_nonempty_days >= min_nonempty_days
  out$pass_all_weekdays <- if (require_all_weekdays) {
    out$has_monday & out$has_tuesday & out$has_wednesday &
      out$has_thursday & out$has_friday & out$has_saturday & out$has_sunday
  } else {
    TRUE
  }
  out$pass_qc <- out$pass_min_days & out$pass_all_weekdays
  names(out)[names(out) == "participant_id"] <- participant_col
  tibble::as_tibble(out)
}

#' Filter APP Usage records to participants passing QC
#'
#' @param data Day-level APP Usage data.
#' @param qc_table Output from `qc_appusage_day()`.
#'
#' @return Filtered `data`.
#' @export
filter_valid_appusage <- function(data, qc_table) {
  if (!"pass_qc" %in% names(qc_table)) {
    cli::cli_abort("`qc_table` must contain a `pass_qc` column.")
  }
  participant_col <- names(qc_table)[[1]]
  if (!participant_col %in% names(data)) {
    cli::cli_abort("`data` must contain participant column {.field {participant_col}}.")
  }
  valid_ids <- as.character(qc_table[[participant_col]][qc_table$pass_qc])
  data[as.character(data[[participant_col]]) %in% valid_ids, , drop = FALSE]
}

standardize_qc_input <- function(data, participant_col, date_col, duration_col) {
  is_all <- if ("is_all_apps" %in% names(data)) {
    data$is_all_apps %in% TRUE
  } else {
    standardize_package_name(data$package_name) == "ALL" | data$app_name == "\\u6240\\u6709\\u5e94\\u7528"
  }
  is_collection <- if ("is_collection_app" %in% names(data)) {
    data$is_collection_app %in% TRUE
  } else if ("package_name" %in% names(data)) {
    standardize_package_name(data$package_name) == "com.w.appusage"
  } else {
    rep(FALSE, nrow(data))
  }

  data.frame(
    participant_id = as.character(data[[participant_col]]),
    date = safe_as_date(data[[date_col]]),
    duration_ms = parse_ms_value(data[[duration_col]]),
    is_all_apps = is_all,
    is_collection_app = is_collection,
    stringsAsFactors = FALSE
  )
}

appusage_daily_totals <- function(data,
                                  use_all_apps_row = FALSE,
                                  include_collection_app = TRUE,
                                  drop_likely_total_all_rows = TRUE,
                                  all_row_tolerance = 0.10) {
  dates <- sort(unique(stats::na.omit(data$date)))
  rows <- lapply(dates, function(date) {
    day <- data[data$date == date, , drop = FALSE]
    all_rows <- day[day$is_all_apps, , drop = FALSE]
    app_rows <- day[!day$is_all_apps, , drop = FALSE]

    if (!include_collection_app) {
      app_rows <- app_rows[!app_rows$is_collection_app, , drop = FALSE]
    }

    if (use_all_apps_row && nrow(all_rows) > 0) {
      total <- max(all_rows$duration_ms, na.rm = TRUE)
      all_used <- TRUE
    } else {
      likely_all <- likely_total_all_rows(
        all_rows$duration_ms,
        app_rows$duration_ms,
        tolerance = all_row_tolerance
      )
      if (drop_likely_total_all_rows && likely_all) {
        total <- sum(app_rows$duration_ms, na.rm = TRUE)
      } else {
        total <- sum(c(app_rows$duration_ms, all_rows$duration_ms), na.rm = TRUE)
      }
      all_used <- FALSE
    }

    if (!is.finite(total)) {
      total <- NA_real_
    }
    data.frame(
      date = date,
      total_duration_ms = total,
      used_all_apps_row = all_used,
      stringsAsFactors = FALSE
    )
  })

  if (length(rows) == 0) {
    return(data.frame(
      date = as.Date(character()),
      total_duration_ms = numeric(),
      used_all_apps_row = logical()
    ))
  }
  do.call(rbind, rows)
}

likely_total_all_rows <- function(all_duration, app_duration, tolerance = 0.10) {
  all_duration <- all_duration[!is.na(all_duration)]
  app_duration <- app_duration[!is.na(app_duration)]
  if (length(all_duration) == 0 || length(app_duration) == 0) {
    return(FALSE)
  }
  all_value <- max(all_duration, na.rm = TRUE)
  app_sum <- sum(app_duration, na.rm = TRUE)
  if (app_sum == 0) {
    return(all_value == 0)
  }
  all_value >= 0 && all_value <= app_sum * (1 + tolerance)
}

weekday_flags <- function(weekdays) {
  all_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  stats::setNames(all_days %in% weekdays, all_days)
}

sort_weekdays <- function(x) {
  order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  x[order(match(x, order), na.last = NA)]
}
