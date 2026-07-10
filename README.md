appusageR
================

`appusageR` parses, validates, standardizes, and summarizes exported APP
Usage / Screen Time Android smartphone-use logs for reproducible
research workflows. Version 0.3.3 provides faithful raw parsers,
BIDS-like per-source cache files, second-level event/episode/daily
outputs, breakpoint-aware resume and rerun support, memory-aware
parallel batch workflows, manual app-category enrichment, project-level
Wenjuanxing/self-report matching, and structured workflow diagnostics.

Author: Kunru Song (<Kunrusong97@gmail.com>)

The package parses files that already exist; it is not an Android data
collector and does not implement questionnaire CIER detection.

APP Usage exports measure Android foreground-use duration from Android
usage statistics. They should not be interpreted directly as attention,
engagement, or subjective involvement with an app.

The 0.3.3 preprocessing core is designed around auditable per-source
caches:

- raw parsers remain faithful to APP Usage export content;
- `duration_ms` and millisecond timestamp fields are the numeric sources
  of truth;
- formatted duration text is retained only as audit/display information;
- `package_name` is the stable app identity key;
- `app_name` is retained for human inspection and reporting;
- diagnostics and category-enrichment outputs are metadata by default,
  not row-deletion rules.

The package does not scrape app stores and does not infer sensitive app
categories automatically.

## Installation

``` r
# Install from GitHub
install.packages("devtools")
devtools::install_github("fenmeng123/appusageR")
```

## Detect Export Type

``` r
library(appusageR)

detect_appusage_type("participant_AppUsage_line.txt")
detect_appusage_type("participant_AppUsage_meta.txt")
detect_appusage_type("participant_AppUsage_day.txt")
detect_appusage_type("participant_AppUsage_app.txt")
```

## Parse Individual Files

``` r
line_records <- parse_line("participant_AppUsage_line.txt", participant_id = "p001")
meta_records <- parse_meta("participant_AppUsage_meta.txt", participant_id = "p001")
day_records <- parse_day("participant_AppUsage_day.txt", participant_id = "p001")
app_records <- parse_app("participant_AppUsage_app.txt", participant_id = "p001")
```

`parse_meta()` returns `list(summary = ..., events = ...)`. The other
parsers return tibbles. Rows are sorted chronologically where the export
contains timestamps or dates.

## Module-Level Workflow

The preferred 0.3.3 user-facing entry points are:

- `read_appusage_text()` for raw text I/O;
- `run_first_level_appusage()` for one source file or text object;
- `run_second_level_appusage()` for one first-level result/cache;
- `run_appusage_workflow()` for the full serial cache workflow;
- `run_appusage_project_workflow()` for project-level preprocessing and
  Wenjuanxing/self-report matching.

Existing lower-level parser and batch functions remain exported for
advanced or developer-facing use.

## Batch Preprocessing

For large studies, use `read_appusage_batch()` to write per-file `.rda`
caches and invisibly return a detailed summary table.

``` r
files <- list.files("raw_exports", pattern = "\\.txt$", full.names = TRUE)

batch_summary <- read_appusage_batch(
  files,
  output_dir = "output_cache",
  overwrite = FALSE,
  progress = TRUE
)
```

Each source file produces two first-level files:

- a BIDS-like metadata JSON file;
- a BIDS-like RDA data file.

The RDA file contains exactly one object named `data`.

The returned `batch_summary` records detected type, status, metadata
path, data path, row counts, parse-warning counts, warnings, error
messages, error classes, calls, traceback text, and timing.

Local reference directories are not package fixtures. In particular,
`reference/all_text_data/` is not used in examples or tests and should
remain untouched unless a full-data stress test is explicitly
authorized.

## Breakpoint-Aware Resume and Rerun

``` r
project <- run_appusage_project_workflow(
  raw_data_root = "raw_project_exports",
  output_root = "project_cache",
  project_id = "123456789",
  upload_col = "uploaded_file",
  resume = TRUE,
  overwrite = FALSE,
  first_level_checkpoint_every = 100,
  retry_memory_allocation = TRUE,
  memory_retry_workers = 1
)
```

Version 0.3.3 can resume interrupted project workflows without treating
a missing final summary as proof that all first-level work must be
repeated. When `resume = TRUE`, the workflow can rebuild first-level
state from existing `proclevel-1` JSON/RDA cache metadata, distinguish
successful caches from recorded errors or incomplete cache pairs, and
continue missing or retryable files instead of reparsing valid
successes.

First-level batches write durable checkpoint summaries during long runs.
Memory allocation failures are classified separately from malformed
content and can be retried with a reduced worker count. Existing
complete second-level caches are skipped when `resume = TRUE` and
`overwrite = FALSE`, while incomplete or selected rows can be rebuilt
through the same project cache layout.

## Parallel Batch Workflow

``` r
files <- list.files("raw_exports", pattern = "\\.txt$", full.names = TRUE)

batch_summary <- read_appusage_batch(
  files,
  output_dir = "output_cache",
  parallel = TRUE,
  n_cores = 12,
  max_workers = 12,
  checkpoint_every = 100,
  resume = TRUE,
  progress = TRUE
)

second_level <- write_second_level_batch(
  batch_summary,
  parallel = TRUE,
  n_cores = 12,
  resume = TRUE
)
```

The first-level worker selector is adaptive. It considers the requested
worker count, available cores, source file count, source file sizes, and
detectable memory information, then records the selected worker count
and cap reason in the returned summary and workflow configuration. Users
can explicitly request an override with `worker_cap_override = TRUE` or
`first_level_worker_cap_override = TRUE` after accepting the memory
risk.

Parallel workers return structured status rows to the main R process.
The summary keeps task index, worker PID, stage, error class, error
message, diagnostic report path, retry metadata, and final status where
available. This keeps long batch runs auditable while preserving
per-source cache files and returned summary order.

## Second-Level Cache Workflow

``` r
second_level <- write_second_level_batch(batch_summary, overwrite = TRUE)
```

Second-level RDA files contain `data$event`, `data$episode`, and
`data$daily`. Each successful second-level cache has a matching
`proc-2.json` metadata file. Second-level summary tables are refreshed
from metadata and worker result rows without loading every RDA payload
into memory.

Supported second-level grains by export type are:

- line exports: `data$episode` and line-episode-derived `data$daily`;
- meta exports by default: faithful `data$event` and Table 1
  summary-derived `data$daily`;
- meta exports with explicit reconstruction: opt-in `data$episode`, and
  episode-derived or combined daily rows when `meta_daily_source` is
  requested;
- day and app exports: `data$daily`.

Unavailable grains are represented by zero-row tables so downstream code
can bind or inspect the same top-level elements. Each second-level RDA
still contains exactly one object named `data`.

Second-level tables include `activity_type` where app identity is
available. The value is `"background"` when APP Usage app labels contain
the background / streaming marker and `"foreground"` otherwise. This is
a flag only; it does not drop rows or alter duration calculations.

## Explicit Meta Reconstruction

`parse_meta()` is faithful to the raw meta export and returns
`list(summary = ..., events = ...)`. It never reconstructs episodes.

Meta event-to-episode reconstruction is explicit and opt-in:

``` r
episodes <- reconstruct_meta_episodes(meta_records$events)

second_level_meta <- run_second_level_appusage(
  first_level_meta,
  reconstruct_meta = TRUE,
  meta_daily_source = "both"
)
```

`meta_daily_source = "summary"` is the default and keeps daily rows
based on Table 1 summary data. `"episodes"` and `"both"` require
explicit reconstruction and retain provenance through daily-source and
duration-comparison columns.

## Manual App Categories

``` r
dict <- read_app_category_dictionary(
  "apptypedict/apptyp_dictionary_v251016.xlsx"
)

category_summary <- write_app_categories_batch(
  unique(batch_summary$project_root),
  dictionary = dict,
  overwrite = TRUE
)
```

Category enrichment writes `Level_1_Category` and `Level_2_Category`
back into second-level `proc-2.rda` files and records match diagnostics
in `proc-2.json`. Matching uses `package_name` against dictionary
`App_UUID` first, then exact unambiguous `app_name` matches against
`App_Name_Repaired` and `App_Name`. It does not infer sensitive
categories or use fuzzy matching.
