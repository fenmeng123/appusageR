from implement_structures_035 import read,write,get,replace

text=read('second_level.R')
f=get(text,'aggregate_meta_episodes_daily')
start=f.index('  rows <- lapply(groups, function(idx) {')
end=f.index('  out <- add_duration_anomalies',start)
f=f[:start]+'''  valid <- meta$reconstruction_status == "complete" & !is.na(meta$duration_ms) & meta$duration_ms >= 0
  valid[is.na(valid)] <- FALSE
  count <- function(x) vapply(groups, function(i) sum(x[i], na.rm = TRUE), integer(1))
  complete_count <- count(valid)
  duration <- vapply(groups, function(i) {
    if (any(valid[i])) sum(meta$duration_ms[i][valid[i]], na.rm = TRUE) else NA_real_
  }, numeric(1))
  date <- as.Date(vapply(groups, function(i) as.numeric(first_nonmissing(meta$date[i])), numeric(1)), origin = "1970-01-01")
  first_text <- function(column) vapply(groups, function(i) first_nonmissing_character(meta[[column]][i]), character(1))
  starts <- count(meta$unmatched_start)
  ends <- count(meta$unmatched_end)
  invalid <- count(meta$reconstruction_status == "invalid_pair")
  warnings <- count(!is.na(meta$reconstruction_warning) & meta$reconstruction_warning != "")
  diagnostics <- count(meta$anomaly_any) + starts + ends + invalid + warnings
  out <- tibble::tibble(
    date = unname(date), weekday = weekday_name(unname(date)),
    app_name = unname(first_text("app_name")), activity_type = unname(first_text("activity_type")),
    package_name = unname(first_text("package_name")), duration_ms = unname(duration),
    duration_min = unname(duration) / 60000, open_count = NA_integer_, notification_count = NA_integer_,
    split_screen_ms = NA_real_, episode_count = unname(complete_count), event_count = NA_integer_,
    source_export_type = "meta", daily_source = "meta_episodes", summary_duration_ms = NA_real_,
    episode_duration_ms = unname(duration), duration_diff_ms = NA_real_, duration_diff_pct = NA_real_,
    duration_agreement_status = "episode_only", complete_episode_count = unname(complete_count),
    unmatched_start_count = unname(starts), unmatched_end_count = unname(ends),
    invalid_pair_count = unname(invalid), reconstruction_warning_count = unname(warnings),
    is_all_apps = FALSE, is_collection_app = unname(vapply(groups, function(i) any(meta$is_collection_app[i], na.rm = TRUE), logical(1))),
    parse_warning = unname(vapply(groups, function(i) compact_character_values(c(meta$parse_warning[i], meta$reconstruction_warning[i])), character(1))),
    n_anomalies = as.integer(diagnostics)
  )
'''+f[end:]
write('second_level.R',replace(text,'aggregate_meta_episodes_daily',f))

text=read('provenance.R').replace('"appusage_line_structural_quality"','"line_structural_quality"').replace('"clip_meta_episode_timeline"','"clip_overlapping_meta_timeline"')
text=text.replace('"parse_line_block", "line_structural_quality"','''"parse_line_block", "line_structural_quality", "appusage_parse_context",
    "appusage_context_row_text", "appusage_context_column", "appusage_context_hits",
    "appusage_context_records", "appusage_prepare_source", "appusage_structural_boundaries",
    "appusage_parse_line_context", "appusage_parse_meta_context",
    "appusage_parse_day_context", "appusage_parse_app_context"''')
text=text.replace('"appusage_validate_second_level_daily", "appusage_order_daily"','''"appusage_validate_second_level_daily", "appusage_order_daily",
    "appusage_meta_pair_indices", "appusage_meta_rows_from_indices",
    "appusage_meta_merge_edges", "merge_contiguous_meta_episodes",
    "appusage_source_qc_interval_segments", "appusage_midnight_ms", "appusage_timezone_names"''')
write('provenance.R',text)
