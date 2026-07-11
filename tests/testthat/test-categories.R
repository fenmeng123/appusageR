category_dictionary <- function() {
  tibble::tibble(
    App_Name = c(
      "Package App", "Internal Repaired", "Visible Name", "Original Only",
      "Conflict A", "Conflict B", "Same A", "Same A"
    ),
    App_Name_Repaired = c(
      NA, "Visible Name", NA, NA,
      NA, NA, NA, NA
    ),
    App_UUID = c(
      "com.pkg.match", "uuid.repaired", "uuid.original", "uuid.original.only",
      "com.conflict", "com.conflict", "com.same", "com.same"
    ),
    Level_1_Category = c(
      "Utilities_and_Tools", "Social_Networking", "Video_Gaming",
      "Information_Seeking", "Music_and_Audio", "Video_Streaming",
      "Shopping_and_Lifestyle", "Shopping_and_Lifestyle"
    ),
    Level_2_Category = c(
      "Tools", "Social", "Gaming", "Search",
      "Music", "Video", "Shopping", "Shopping"
    )
  )
}

category_frame <- function() {
  tibble::tibble(
    app_name = c(
      "Whatever", "Visible Name", "Original Only", "Conflict A",
      "Same A", "No Match"
    ),
    package_name = c(
      "COM.PKG.MATCH", "missing.repaired", "missing.original",
      "com.conflict", "com.same", "missing.none"
    )
  )
}

test_that("read_app_category_dictionary validates required columns", {
  bad <- category_dictionary()
  bad$App_UUID <- NULL

  expect_error(
    add_app_categories(category_frame(), bad),
    "missing required column"
  )
})

test_that("add_app_categories uses all keys and handles conflicts", {
  out <- add_app_categories(category_frame(), category_dictionary())

  expect_equal(out$Level_1_Category[[1]], "Utilities_and_Tools")
  expect_equal(out$app_category_match_method[[1]], "app_uuid")

  expect_equal(out$Level_1_Category[[2]], "Social_Networking")
  expect_equal(out$app_category_match_method[[2]], "app_name_repaired")

  expect_equal(out$Level_1_Category[[3]], "Information_Seeking")
  expect_equal(out$app_category_match_method[[3]], "app_name")

  expect_true(is.na(out$Level_1_Category[[4]]))
  expect_equal(out$app_category_match_status[[4]], "conflict")
  expect_equal(out$app_category_match_method[[4]], "app_uuid_conflict")

  expect_equal(out$Level_1_Category[[5]], "Shopping_and_Lifestyle")
  expect_equal(out$app_category_match_method[[5]], "app_uuid")

  expect_true(is.na(out$Level_1_Category[[6]]))
  expect_equal(out$app_category_match_status[[6]], "unmatched")
})

test_that("repaired names take priority over original-name fallback", {
  out <- add_app_categories(
    tibble::tibble(app_name = "Visible Name", package_name = "missing"),
    category_dictionary()
  )

  expect_equal(out$Level_1_Category[[1]], "Social_Networking")
  expect_equal(out$app_category_match_method[[1]], "app_name_repaired")
})

test_that("add_app_categories works on second-level data lists", {
  data <- list(
    event = tibble::tibble(
      app_name = "Whatever",
      package_name = "com.pkg.match"
    ),
    episode = tibble::tibble(
      app_name = "Visible Name",
      package_name = "missing"
    ),
    daily = tibble::tibble(app_name = "No Match", package_name = "missing.none")
  )

  out <- add_app_categories(data, category_dictionary())
  summary <- attr(out, "app_category_summary")

  expect_equal(out$event$Level_1_Category[[1]], "Utilities_and_Tools")
  expect_equal(out$episode$Level_1_Category[[1]], "Social_Networking")
  expect_true(is.na(out$daily$Level_1_Category[[1]]))
  expect_equal(summary$n_category_rows, 3)
  expect_equal(summary$n_category_matched_rows, 2)
})

test_that("write_app_categories_batch overwrites proc-2 only", {
  path <- testthat::test_path("fixtures", "line_sample.txt")
  parent_dir <- file.path(
    tempdir(),
    paste0("appusage_cat_project_", as.integer(runif(1, 1, 1e8)))
  )
  first <- read_appusage_batch(
    path,
    ids = "line_id",
    output_dir = parent_dir,
    project_name = "CatFull",
    project_id = "e5f6",
    progress = FALSE
  )
  project <- unique(first$project_root)
  second <- write_second_level_batch(first, overwrite = TRUE, progress = FALSE)
  creator_metadata <- jsonlite::read_json(
    second$metadata_json[[1]], simplifyVector = TRUE
  )
  creator_provenance <- creator_metadata$implementation_provenance

  dict <- tibble::tibble(
    App_Name = "系统桌面",
    App_Name_Repaired = NA_character_,
    App_UUID = "com.miui.home",
    Level_1_Category = "Utilities_and_Tools",
    Level_2_Category = "System Tools"
  )
  category_summary <- write_app_categories_batch(
    project,
    dict,
    progress = FALSE
  )

  expect_equal(category_summary$status[[1]], "success")
  expect_gt(category_summary$n_category_matched_rows[[1]], 0)

  env <- new.env(parent = emptyenv())
  loaded <- load(second$second_level_data_file[[1]], envir = env)
  expect_identical(loaded, "data")
  expect_true(all(
    c("Level_1_Category", "Level_2_Category") %in%
      names(env$data$episode)
  ))
  expect_equal(env$data$episode$Level_1_Category[[1]], "Utilities_and_Tools")
  expect_equal(env$data$daily$Level_2_Category[[1]], "System Tools")

  first_env <- new.env(parent = emptyenv())
  load(first$data_file[[1]], envir = first_env)
  expect_false("Level_1_Category" %in% names(first_env$data$line))

  proc2_metadata <- jsonlite::read_json(
    second$metadata_json[[1]],
    simplifyVector = TRUE
  )
  expect_equal(proc2_metadata$processing$app_category_status, "success")
  expect_equal(proc2_metadata$category_dictionary$n_category_matched_apps, 2)
  expect_identical(
    proc2_metadata$implementation_provenance$workflow_run_id,
    creator_provenance$workflow_run_id
  )
  expect_identical(
    proc2_metadata$implementation_provenance$parser_implementation_fingerprint,
    creator_provenance$parser_implementation_fingerprint
  )

  proc2 <- utils::read.csv(file.path(
    project,
    "analytic_summary_table_proclevel-2.csv"
  ))
  expect_true("n_category_matched_rows" %in% names(proc2))
  expect_gt(proc2$n_category_matched_rows[[1]], 0)
  expect_false(dir.exists(file.path(project, "proclevel-3")))
})

test_that("QC can run after category enrichment", {
  path <- testthat::test_path("fixtures", "line_sample.txt")
  parent_dir <- file.path(
    tempdir(),
    paste0("appusage_cat_qc_", as.integer(runif(1, 1, 1e8)))
  )
  first <- read_appusage_batch(
    path,
    ids = "line_id",
    output_dir = parent_dir,
    project_name = "CatQC",
    project_id = "a7b8",
    progress = FALSE
  )
  project <- unique(first$project_root)
  write_second_level_batch(first, overwrite = TRUE, progress = FALSE)

  dict <- tibble::tibble(
    App_Name = "系统桌面",
    App_Name_Repaired = NA_character_,
    App_UUID = "com.miui.home",
    Level_1_Category = "Utilities_and_Tools",
    Level_2_Category = "System Tools"
  )
  write_app_categories_batch(project, dict, progress = FALSE)

  qc <- write_qc_metadata_batch(project, progress = FALSE)
  expect_false(dir.exists(file.path(project, "proclevel-3")))
  expect_equal(qc$qc_status[[1]], "success")
  expect_equal(qc$app_category_status[[1]], "success")
})
