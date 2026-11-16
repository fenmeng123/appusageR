# Synthetic study

All logs, identities and questionnaire responses in this directory are fictional.
They were constructed deterministically for appusageR teaching and are distributed
under GPL-3. Log dates are 1–7 October 2025 in **Asia/Shanghai**; native numeric
timestamps are milliseconds. ProjectID **009** is a string.

## Files and scenarios

The project directory contains 15 TXT uploads for 14 fictional sequence identities.
`scenarios.csv` gives per-file expected counts and hand-calculated totals.

| Sequence | Situation | Expected interpretation |
|---|---|---|
| 101 | Seven-day line export, two apps | 14 episodes; 210 minutes; default day-QC passes |
| 102 | Seven-day meta tables 1 and 2 | 28 events, 14 reconstructed episodes; 140 minutes |
| 103 | Seven-day day export with ALL totals | 21 daily rows remain; app-only total 105 minutes, not 210 |
| 104 | Seven-day app export for one app | 105 recorded minutes; coverage is limited to that app |
| 105 | Mixed line/meta content in a file named meta | 28 events, 14 native line episodes; 210 minutes, without adding meta totals |
| 106 | Recorded, explicitly empty and absent days | Oct 1/4 recorded, Oct 2 empty, Oct 3 absent; total 5 minutes |
| 107 | Interval crossing midnight | Oct 1 receives 5 minutes; Oct 2 receives 5 + 2 minutes |
| 108 | Six overlapping intervals | 44 seconds summed; 14-second union; 30 seconds excess |
| 109 | Two uploads for one identity | Both caches retained; the later export is nearest the survey submission |
| 110 | Empty mixed export | Header-only input failure, retained for recovery |
| 111 | Unrelated text | Unknown-content input failure |
| 112 | Zero-byte upload | Zero-byte input failure |
| 113 | Artificial PNG signature with a TXT extension | Binary input failure; these bytes are not an actual image |
| 114 | One interval lacks its end timestamp | Critical structural-quality input failure |

The first five sources cover all seven weekdays and pass the existing day-QC
criteria. The other five usable sources have too few nonempty days. Input failures
have no proc-2 data and are not part of that QC denominator. QC passage does not
make every scientific grain available: day/app exports remain daily-only.

The six overlap offsets are `(0,10), (0,10), (0,6), (4,10), (3,7), (6,14)` seconds,
with distinct fictional app identities. The mixed file retains configuration text
containing a comma. Names and package identifiers are fictional and do not imply
coverage by the bundled manual category dictionary.

## Run the project and link the questionnaire

```r
library(appusageR)
input <- system.file("extdata", "synth", package = "appusageR",
                     mustWork = TRUE)
output <- tempfile("synthetic-study-")
result <- run_appusage_project_workflow(
  raw_data_root = input, project_id = "009", output_root = output,
  self_report_sheet = "Survey", sequence_col = "sequence_id",
  upload_col = "upload", submit_time_col = "submitted_at",
  parallel = TRUE, n_cores = 2L, progress = FALSE, strict = FALSE
)
data.frame(source = basename(result$first_level$source_file),
           status = result$first_level$status,
           worker_pid = result$first_level$worker_pid)
result$qc[, c("participant_id", "qc_status", "pass_qc")]
result$matched_self_report[, c("survey_id", "moSens_match_status",
                               "moSens_data_dir")]
# After inspecting the temporary outputs:
# unlink(output, recursive = TRUE)
```

Alternatively, run `demo("synthetic-project", package = "appusageR", ask = FALSE)`.
It leaves `demo_result` and `demo_output` available for inspection. The example
processes files within one project in parallel. Small inputs teach scheduling;
worker startup can cost more than the work itself. Use `parallel = FALSE,
n_cores = 1L` for a serial run. Memory-aware worker limits remain active.

`strict = FALSE` lets the deliberate input failures remain in the batch's
diagnostics. The expected outcome is **10 successful sources and 5 input failures**,
with day-QC **5 pass / 5 fail among the 10 successful sources**.

The workbook's first sheet, `Survey`, has 17 observations. It includes text
sequence IDs, a separate survey row ID, age, sleep hours, self-reported phone
minutes, uploaded filenames and an Excel date-time submission column. `Fields`
documents the variables. `survey.csv` preserves the same data using ISO local
date-time strings; one missing sleep response and a genuine zero are distinct.
Self-report values were chosen independently of the logs.

Expected linking results are **8 matched**, **5 unmatched_no_proc2**, and one each
of **unmatched_filename**, **unmatched_sequence**, **unmatched_no_upload_filename**
and **unmatched**. All 17 rows remain in order. Sequence 108 deliberately lists
the wrong export time; 201 references an absent file; 202 has no upload; 203
references 101's filename with the wrong sequence. `expected-links.csv` specifies
each row. Sequence 109's resolution uses the package's existing export-type and
nearest-submission-time rules, not a study-specific reupload preference.

## Inspect individual formats

`data(appusage_demo)` loads the standardized results for the ten successful
sources, with their coverage/export metadata, this source inventory and the
questionnaire. Inspect `appusage_demo$records$mixed_line_meta$data` for the three
grains. `?appusage_demo` documents the object and its installed reproduction
script. The remaining five sources stay in the inventory with their expected
failure classes.

```r
scenarios <- read.csv(file.path(input, "scenarios.csv"))
project <- file.path(input, "ProjectName-Demo_ProjectID-009")
first <- run_first_level_appusage(file.path(project, scenarios$source_basename[5]),
                                  strict = TRUE)
second <- run_second_level_appusage(first)
c(event = nrow(second$data$event), episode = nrow(second$data$episode),
  daily = nrow(second$data$daily))  # 28, 14, 14
sum(second$data$daily$duration_ms) # 12600000
```

For sequence 106, an explicit Oct 1–4 window gives an observed-day denominator
of 2 and mean 150,000 ms. Explicitly treating the whole window as zero-filled
gives a denominator of 4 and mean 75,000 ms. Both have numerator 300,000 ms.
These are alternate audit denominators; they do not change the saved daily table
or the default seven-day QC requirement.
