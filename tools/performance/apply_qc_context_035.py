"""Apply checked, narrow QC cache plumbing to the development source."""
from pathlib import Path
import re
root = Path(__file__).resolve().parents[2]

def replace(s, old, new, n=1):
    assert s.count(old) == n, (old, s.count(old), n)
    return s.replace(old, new)

p = root / 'R/anomaly_qc.R'
s = p.read_text(encoding='utf-8-sig')
s = replace(s, '  grains <- appusage_normalize_anomaly_input(data)', '  grains <- appusage_normalize_anomaly_input(data)\n  context <- appusage_qc_context(grains, source_qc_config)')
s = replace(s, '    max_episode_ms = max_episode_ms\n  )', '    max_episode_ms = max_episode_ms, context = context\n  )')
s = replace(s, '    meta_diff_ratio = meta_diff_ratio\n  )', '    meta_diff_ratio = meta_diff_ratio, context = context\n  )')
s = replace(s, '    max_export_lookback_days = max_export_lookback_days\n  )', '    max_export_lookback_days = max_export_lookback_days, context = context\n  )')
s = replace(s, '    metadata = metadata\n  )', '    metadata = metadata, context = context\n  )')
s = replace(s, '                                             max_episode_ms) {', '                                             max_episode_ms, context = NULL) {')
s = replace(s, '                                           meta_diff_ratio) {', '                                           meta_diff_ratio, context = NULL) {')
s = replace(s, '    max_daily_total_ms\n  )', '    max_daily_total_ms, context = context\n  )')
s = replace(s, '                                         max_daily_total_ms) {', '                                         max_daily_total_ms, context = NULL) {')
s = replace(s, '                                                 max_export_lookback_days) {', '                                                 max_export_lookback_days, context = NULL) {')
s = replace(s, 'appusage_observed_dates(grains)', 'appusage_observed_dates(grains, context)')
s = replace(s, 'appusage_observed_dates <- function(grains)', 'appusage_observed_dates <- function(grains, context = NULL)')
# Route only functions which received a context; standalone private calls retain fallback behavior.
for name in ['appusage_check_episode_anomalies', 'appusage_check_daily_anomalies', 'appusage_daily_total_by_date', 'appusage_observed_dates']:
    start = s.index(name + ' <- function')
    following = re.search(r'^\w+ <- function', s[start+len(name)+1:], re.M)
    end = start+len(name)+1+following.start() if following else len(s)
    block = s[start:end]
    for grain in ['episode', 'daily', 'event']:
        block = re.sub(r'appusage_date_from_ms\(appusage_num_col\('+grain+r', "(\w+)"\)\)',
            lambda m: f'appusage_qc_date({grain}, "{m[1]}", context, "{grain}", ms = TRUE)', block)
        block = re.sub(r'appusage_date_col\('+grain+r', "(\w+)"\)',
            lambda m: f'appusage_qc_date({grain}, "{m[1]}", context, "{grain}")', block)
        block = re.sub(r'appusage_(num|chr|lgl)_col\('+grain+r', "(\w+)"\)',
            lambda m: f'appusage_qc_col({grain}, "{m[2]}", "{m[1]}", context, "{grain}")', block)
    s = s[:start]+block+s[end:]
p.write_text(s, encoding='utf-8')

p = root / 'R/source_anomaly_qc.R'
s = p.read_text(encoding='utf-8-sig')
s = replace(s, 'appusage_source_anomaly_qc <- function(data, config = NULL, metadata = NULL) {\n  grains <- appusage_normalize_anomaly_input(data)\n  config <- appusage_source_qc_config(config)', '''appusage_source_anomaly_qc <- function(data, config = NULL, metadata = NULL,
                                       context = NULL) {
  if (is.null(context)) {
    grains <- appusage_normalize_anomaly_input(data)
    config <- appusage_source_qc_config(config)
    context <- appusage_qc_context(grains, config)
  } else {
    grains <- context$grains
    config <- context$config
  }''')
for name in ['appusage_line_overlap_qc', 'appusage_line_timestamp_qc']:
    s = replace(s, name+'(grains$episode, config)', name+'(grains$episode, config, context)')
    s = replace(s, name+' <- function(episode, config)', name+' <- function(episode, config, context = NULL)')
s = replace(s, '    grains$daily,\n    config\n  )', '    grains$daily,\n    config, context\n  )')
s = replace(s, 'appusage_meta_reconstruction_qc <- function(episode, daily, config)', 'appusage_meta_reconstruction_qc <- function(episode, daily, config, context = NULL)')
s = replace(s, '  line <- appusage_line_episode_rows(episode)', '  view <- appusage_qc_episode_view(episode, "line", context)\n  line <- view$data\n  rows <- view$rows', 2)
s = replace(s, '  source <- appusage_chr_col(episode, "episode_source")\n  meta <- episode[!is.na(source) & source == "meta_events", , drop = FALSE]', '  view <- appusage_qc_episode_view(episode, "meta", context)\n  meta <- view$data\n  rows <- view$rows')
s = replace(s, '  valid <- !is.na(start) & !is.na(end) & !is.na(duration) &\n    end >= start & duration >= 0', '  valid <- appusage_qc_valid_intervals(start, end, duration, context, rows)')
s = replace(s, '  valid <- !is.na(start) & !is.na(end) & !is.na(duration) & end >= start & duration >= 0', '  valid <- appusage_qc_valid_intervals(start, end, duration, context, rows)')
s = replace(s, 'appusage_source_qc_interval_segments(fg, config$effective_timezone)', 'appusage_source_qc_interval_segments(fg, config$effective_timezone, context, rows[foreground])')
s = replace(s, 'appusage_source_qc_interval_segments(eligible, config$effective_timezone)', 'appusage_source_qc_interval_segments(eligible, config$effective_timezone, context, rows[complete])')
s = replace(s, 'appusage_source_qc_interval_segments <- function(x, tz)', 'appusage_source_qc_interval_segments <- function(x, tz, context = NULL, rows = NULL)')
s = replace(s, '''  tz <- appusage_resolve_timezone(tz)
  start <- as.numeric(x$start_ts_ms)
  end <- as.numeric(x$end_ts_ms)
  duration <- as.numeric(x$duration_ms)
  calendar <- appusage_interval_calendar(start, end, tz)''', '''  if (is.null(context)) tz <- appusage_resolve_timezone(tz)
  start <- if (is.null(context)) as.numeric(x$start_ts_ms) else
    appusage_qc_col(x, "start_ts_ms", "num", context, rows = rows)
  end <- if (is.null(context)) as.numeric(x$end_ts_ms) else
    appusage_qc_col(x, "end_ts_ms", "num", context, rows = rows)
  duration <- if (is.null(context)) as.numeric(x$duration_ms) else
    appusage_qc_col(x, "duration_ms", "num", context, rows = rows)
  calendar <- if (is.null(context)) appusage_interval_calendar(start, end, tz) else list(
    start_date = appusage_qc_date(x, "start_ts_ms", context, rows = rows, tz = tz, ms = TRUE),
    end_date = appusage_qc_date(x, "end_ts_ms", context, rows = rows, tz = tz, ms = TRUE, offset = -0.001)
  )''')
for name, var in [('appusage_line_overlap_qc','line'), ('appusage_line_timestamp_qc','line'), ('appusage_meta_reconstruction_qc','meta')]:
    start = s.index(name + ' <- function')
    following = re.search(r'^\w+ <- function', s[start+len(name)+1:], re.M)
    end = start+len(name)+1+following.start() if following else len(s)
    block = s[start:end]
    block = re.sub(r'appusage_(num|chr|lgl)_col\(\s*'+var+r', "(\w+)"\s*\)',
        lambda m: f'appusage_qc_col({var}, "{m[2]}", "{m[1]}", context, rows = rows)', block)
    block = block.replace('appusage_date_from_datetime(ms = start, tz = config$effective_timezone)',
        'appusage_qc_date(line, "start_ts_ms", context, rows = rows, tz = config$effective_timezone, ms = TRUE)')
    block = block.replace('appusage_date_from_datetime(ms = end, tz = config$effective_timezone)',
        'appusage_qc_date(line, "end_ts_ms", context, rows = rows, tz = config$effective_timezone, ms = TRUE)')
    block = block.replace('appusage_date_col(line, "source_table_date")',
        'appusage_qc_date(line, "source_table_date", context, rows = rows)')
    s = s[:start]+block+s[end:]
p.write_text(s, encoding='utf-8')

p = root / 'R/provenance.R'
s = p.read_text(encoding='utf-8-sig')
s = replace(s, '    "appusage_date_from_datetime_validated"\n', '''    "appusage_date_from_datetime_validated",
    "appusage_qc_context", "appusage_qc_col", "appusage_qc_date",
    "appusage_qc_episode_view", "appusage_qc_valid_intervals",
    "qc_appusage_anomalies", "run_qc_for_second_level_data",
    "appusage_source_anomaly_qc", "appusage_check_episode_anomalies",
    "appusage_check_daily_anomalies", "appusage_daily_total_by_date",
    "appusage_check_export_span_anomalies", "appusage_observed_dates",
    "appusage_line_overlap_qc", "appusage_line_timestamp_qc",
    "appusage_meta_reconstruction_qc"
''')
p.write_text(s, encoding='utf-8')
