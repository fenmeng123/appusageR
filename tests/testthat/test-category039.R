category039_dictionary <- function() data.frame(
  App_Name = c("Alpha", "Beta", "Same One", "Same Two", "Fallback", "Other Fallback"),
  App_Name_Repaired = c(NA, NA, NA, NA, "Visible", NA),
  App_UUID = c("pkg.conflict", "pkg.conflict", "pkg.same", "PKG.SAME", "pkg.name", "pkg.other"),
  Level_1_Category = c("Tools", "Games", "Tools", "Tools", "Games", "Tools"),
  Level_2_Category = c("Utility", "Gameplay", "Utility", "Utility", "Gameplay", "Utility"))

category039_daily <- function(app = "new.pkg", ms = 1, total = 100) {
  data.frame(date = as.Date("2024-01-01"), package_name = c(app, "known.pkg"),
    app_name = c("New app", "Known"), duration_ms = c(ms, total - ms),
    activity_type = "foreground", daily_source = "day_export", source_export_type = "day")
}

test_that("APK disambiguation is restricted and package decisions win", {
  x <- data.frame(package_name = c("pkg.conflict", "pkg.conflict", "pkg.conflict", "pkg.same", "unknown", "ALL", NA),
    app_name = c("Alpha", "Fallback", "Beta", "Visible", "Visible", "ALL", NA),
    duration_ms = seq_len(7))
  y <- add_app_categories(x, category039_dictionary())
  expect_identical(y$app_category_match_status, c("matched", "conflict", "matched", "matched", "matched", "not_app", "unmatched"))
  expect_identical(y$Level_2_Category[c(1, 3, 4, 5)], c("Utility", "Gameplay", "Utility", "Gameplay"))
  expect_true(y$app_category_name_disagreement[[4]])
  expect_identical(y$app_category_disposition[[7]], "missing_identity")
  expect_identical(appusage_without_categories(y), x)
  expect_error(add_app_categories(x, transform(category039_dictionary(), Level_2_Category = NA_character_)), "complete")
  bad <- category039_dictionary(); bad$Level_2_Category[[2]] <- "Utility"
  expect_error(add_app_categories(x, bad), "single Level 1")
  changed <- as_app_category_dictionary(category039_dictionary())
  changed$Level_1_Category[[1]] <- "Modified"
  expect_error(add_app_categories(x, changed), "modified")
  indexed <- as_app_category_dictionary(category039_dictionary())
  attr(indexed, "app_category_lookups")$app_uuid$matches$Level_1_Category[[1]] <- "Modified"
  expect_error(add_app_categories(x, indexed), "modified")
})

test_that("dictionary indices are reused within the process and remain content-bound", {
  dictionary <- as_app_category_dictionary(category039_dictionary())
  build <- build_category_lookup
  local_mocked_bindings(build_category_lookup = function(...) stop("Unexpected index rebuild"))
  expect_identical(build_category_lookups(dictionary), attr(dictionary, "app_category_lookups"))
  altered <- dictionary; altered$.app_uuid_key[[1]] <- "different.key"
  expect_error(build_category_lookups(altered), "Unexpected index rebuild")
})

test_that("profiles deduplicate people and primary observations with OR thresholds", {
  dict <- data.frame(App_Name = "Known", App_UUID = "known.pkg", Level_1_Category = "Tools", Level_2_Category = "Utility")
  ids <- paste0("s", seq_len(201))
  manifest <- data.frame(source_id = ids, person_id = paste0("p", seq_len(201)),
    observation_id = paste0("o", seq_len(201)), selected_primary = TRUE)
  data <- setNames(lapply(seq_len(201), function(i) list(daily = category039_daily(
    if (i == 1) "new.pkg" else "known.pkg", if (i == 1) 1 else 0))), ids)
  profile <- profile_uncoded_apps(rbind(manifest, manifest[1, ]), dict, data = data)
  new <- profile$apps[profile$apps$app_key == "pkg:new.pkg", ]
  expect_equal(new$n_people_denominator, 201)
  expect_equal(new$n_people_positive, 1)
  expect_identical(new$disposition, "auto_other")
  x <- add_app_categories(category039_daily(), dict, profile = profile)
  expect_identical(x$Level_1_Category[[1]], "Others")
  expect_identical(x$Level_2_Category[[1]], "Unclassified")
  expect_identical(x$app_category_match_status[[1]], "unmatched")
  data$s1$daily$duration_ms <- c(5, 95)
  high <- profile_uncoded_apps(manifest, dict, data = data)
  expect_identical(high$apps$disposition[high$apps$app_key == "pkg:new.pkg"], "needs_review")
  boundary <- profile_uncoded_apps(manifest[1:100, ], dict, data = data)
  expect_equal(boundary$apps$prevalence[boundary$apps$app_key == "pkg:new.pkg"], .01)
  extra <- manifest[1, ]; extra$source_id <- "alternate"; extra$observation_id <- "o-alt"; extra$selected_primary <- FALSE
  data$alternate <- list(daily = category039_daily("alternate.pkg", 99))
  alt <- profile_uncoded_apps(rbind(manifest, extra), dict, data = data)
  expect_identical(alt$apps$disposition[alt$apps$app_key == "pkg:alternate.pkg"], "insufficient_metrics")
  extra$selected_primary <- TRUE; extra$person_id <- "p1"
  data$alternate <- list(daily = category039_daily("new.pkg", 25))
  repeated <- profile_uncoded_apps(rbind(manifest, extra), dict, data = data)
  rr <- repeated$apps[repeated$apps$app_key == "pkg:new.pkg", ]
  expect_equal(rr$n_people_denominator, 201)
  expect_equal(rr$n_people_positive, 1)
  expect_equal(rr$max_person_share, .25)
  profile$apps$disposition[profile$apps$app_key == "pkg:new.pkg"] <- "needs_review"
  expect_error(add_app_categories(category039_daily(), dict, profile = profile), "does not match")
})

test_that("a profile cannot silently classify sources outside its frozen corpus", {
  dict <- data.frame(App_Name = "Known", App_UUID = "known.pkg", Level_1_Category = "Tools", Level_2_Category = "Utility")
  manifest <- data.frame(source_id = "s", person_id = "p", observation_id = "o", selected_primary = TRUE)
  f <- category039_daily("absent.pkg"); f$app_name <- "Known"
  profile <- profile_uncoded_apps(manifest, dict, data = list(s = list(daily = f)))
  f$app_name <- "Unrecognized alias"
  out <- add_app_categories(f, dict, profile = profile)
  expect_identical(out$app_category_disposition[[1]], "needs_profile")
  expect_true(is.na(out$Level_1_Category[[1]]))
  expect_error(update_app_categories_cache(data.frame(rda_path = "missing.rda", source_id = "outside"),
    dict, profile = profile, progress = FALSE), "membership")
  profile$manifest$person_id <- "wrong"
  expect_error(add_app_categories(f, dict, profile = profile), "does not match")
})

test_that("canonical measurements and extraction do not double count grains", {
  daily <- category039_daily()
  x <- list(event = daily, episode = daily, daily = daily)
  m <- appusage_category_measure_source(x, "s")
  expect_equal(sum(m$metrics$duration_ms), 100)
  uncoded <- extract_uncoded_apps(x)
  expect_equal(sum(uncoded$total_duration_ms), 100)
  meta <- x; meta$daily$source_export_type <- "meta"; meta$daily$daily_source <- "meta_summary"
  attr(meta, "daily_aggregation_expected") <- list(meta_episodes = list(rows = daily))
  expect_equal(sum(appusage_category_measure_source(meta, "s")$metrics$duration_ms), 100)
  daily$duration_ms[[1]] <- NA_real_
  expect_identical(appusage_category_measure_source(list(daily = daily), "s")$status$metric_status, "invalid_foreground_metric")
  expect_equal(nrow(add_app_categories(daily[0, ], category039_dictionary())), 0L)
})

test_that("positive presence differs from explicitly measured zero and missing identity", {
  dict <- data.frame(App_Name = "Known", App_UUID = "known.pkg", Level_1_Category = "Tools", Level_2_Category = "Utility")
  manifest <- data.frame(source_id = "s", person_id = "p", observation_id = "o", selected_primary = TRUE)
  d <- category039_daily(ms = 0)
  profile <- profile_uncoded_apps(manifest, dict, data = list(s = list(daily = d)))
  row <- profile$apps[profile$apps$app_key == "pkg:new.pkg", ]
  expect_equal(row$n_people_positive, 0)
  expect_equal(row$max_person_share, 0)
  expect_identical(row$disposition, "auto_other")
  d$package_name <- NA_character_; d$app_name <- NA_character_
  measured <- appusage_category_measure_source(list(daily = d), "s")
  expect_equal(nrow(measured$metrics), 0)
  expect_identical(measured$status$metric_status, "missing_foreground_identity")
  manifest$person_id <- NA_character_
  pending <- profile_uncoded_apps(manifest, dict, data = list(s = list(daily = category039_daily())))
  unknown <- pending$apps[pending$apps$app_key == "pkg:new.pkg", ]
  expect_identical(unknown$disposition, "insufficient_metrics")
  expect_identical(unknown$review_reason, "insufficient_statistical_identity")
  expect_equal(unknown$n_people_denominator, 0)
  d <- category039_daily(); d$activity_type <- "background"
  expect_identical(appusage_category_measure_source(list(daily = d), "s")$status$metric_status, "no_foreground_records")
})

test_that("category changes do not invalidate questionnaire source selection", {
  project <- tempfile("category039-matching-"); dir.create(project)
  linked <- file.path(project, "saved_proc-2.rda")
  file.create(linked) # Request ownership records paths without decoding RDA.
  first <- data.frame(source_file = "original.txt", data_file = "first.rda")
  second <- data.frame(source_file = "original.txt", first_level_data_file = "first.rda", second_level_data_file = linked)
  a <- appusage_config(matching = list(enabled = TRUE, upload_col = "upload", self_report = data.frame(id = "1")))
  b <- a; b$category <- list(enabled = TRUE, dictionary = category039_dictionary(), policy = appusage_category_policy())
  expect_identical(appusage_matching_request(project, a, first, second),
    appusage_matching_request(project, b, first, second))
})

test_that("flat category-only updates preserve science and resume by content", {
  parent <- tempfile("category039-"); dir.create(parent)
  first <- read_appusage_batch(test_path("fixtures", "line_sample.txt"), ids = "1",
    output_dir = parent, project_name = "Category", project_id = "039", progress = FALSE)
  second <- write_second_level_batch(first, progress = FALSE)
  path <- second$second_level_data_file[[1]]
  raw <- load_appusage_data_object(path)
  json <- second_level_metadata_path(path)
  old_meta <- jsonlite::read_json(json, simplifyVector = FALSE)
  result <- update_app_categories_cache(path, category039_dictionary(), progress = FALSE)
  expect_identical(result$action, "run")
  expect_identical(appusage_without_categories(load_appusage_data_object(path)), raw)
  new_meta <- jsonlite::read_json(json, simplifyVector = FALSE)
  expect_identical(new_meta$qc, old_meta$qc)
  expect_identical(new_meta$identity, old_meta$identity)
  hashes <- tools::md5sum(c(path, json))
  frozen_contract <- appusage_category_contract(category039_dictionary())
  # Mocking the loader changes the recursively collected implementation hash.
  # Hold that already verified contract constant while proving no load occurs.
  local_mocked_bindings(load_appusage_data_object = function(...) stop("Unexpected RDA load"),
    appusage_category_contract = function(...) frozen_contract)
  expect_identical(update_app_categories_cache(path, category039_dictionary(), progress = FALSE)$action, "reuse")
  expect_identical(tools::md5sum(c(path, json)), hashes)
  writeBin(charToRaw("tampered"), path)
  expect_error(update_app_categories_cache(path, category039_dictionary(), progress = FALSE), "intact verified")
})

test_that("bundled dictionary provenance survives JSON contract roundtrips", {
  parent <- tempfile("category039-default-"); dir.create(parent)
  first <- read_appusage_batch(test_path("fixtures", "line_sample.txt"), ids = "1",
    output_dir = parent, project_name = "Default", project_id = "039", progress = FALSE)
  second <- write_second_level_batch(first, progress = FALSE)
  path <- second$second_level_data_file[[1]]
  expect_identical(update_app_categories_cache(path, progress = FALSE)$action, "run")
  hashes <- tools::md5sum(c(path, second_level_metadata_path(path)))
  expect_identical(update_app_categories_cache(path, progress = FALSE)$action, "reuse")
  expect_identical(tools::md5sum(names(hashes)), hashes)
})

test_that("DataBox prefixed files publish to their explicit sibling JSON", {
  parent <- tempfile("category039-flatprefix-"); dir.create(parent)
  first <- read_appusage_batch(test_path("fixtures", "line_sample.txt"), ids = "1",
    output_dir = parent, project_name = "Flat", project_id = "001", progress = FALSE)
  second <- write_second_level_batch(first, progress = FALSE)
  original <- second$second_level_data_file[[1]]
  flat <- file.path(parent, paste0("ProjectID-001__", basename(original)))
  json <- sub("[.]rda$", ".json", flat)
  file.copy(original, flat); file.copy(second_level_metadata_path(original), json)
  before <- load_appusage_data_object(original)
  expect_identical(update_app_categories_cache(flat, progress = FALSE)$action, "run")
  expect_identical(appusage_without_categories(load_appusage_data_object(flat)), before)
  metadata <- jsonlite::read_json(json, simplifyVector = FALSE)
  expect_identical(metadata$outputs$metadata_json, normalized_summary_path(json))
  expect_identical(metadata$outputs$second_level_rda, normalized_summary_path(flat))
  expect_identical(update_app_categories_cache(flat, progress = FALSE)$action, "reuse")
})

test_that("category publication rollback preserves the prior RDA/JSON pair", {
  parent <- tempfile("category039-rollback-"); dir.create(parent)
  first <- read_appusage_batch(test_path("fixtures", "line_sample.txt"), ids = "1",
    output_dir = parent, project_name = "Rollback", project_id = "039", progress = FALSE)
  second <- write_second_level_batch(first, progress = FALSE)
  path <- second$second_level_data_file[[1]]; json <- second_level_metadata_path(path)
  before <- tools::md5sum(c(path, json))
  promote <- appusage_promote_file
  local_mocked_bindings(appusage_promote_file = function(from, to) {
    if (grepl("[.]json[.]app(categories|usage)-tmp", from)) return(FALSE)
    promote(from, to)
  })
  expect_error(update_app_categories_cache(path, category039_dictionary(), progress = FALSE), "promote")
  expect_identical(tools::md5sum(c(path, json)), before)
})

test_that("dictionary-only fresh workflow and postprocessing use the same service", {
  parent <- tempfile("category039-integration-"); dir.create(parent)
  files <- test_path("fixtures", "line_sample.txt")
  config <- appusage_config(category = list(enabled = TRUE, dictionary = category039_dictionary()),
    execution = list(progress = FALSE))
  fresh <- run_appusage_workflow(files, project_dir = file.path(parent, "fresh"), config = config)
  plain <- run_appusage_workflow(files, project_dir = file.path(parent, "plain"),
    config = appusage_config(execution = list(progress = FALSE)))
  fp <- appusage_summary_proc2_paths(fresh$second_level)[[1]]
  pp <- appusage_summary_proc2_paths(plain$second_level)[[1]]
  update_app_categories_cache(pp, category039_dictionary(), progress = FALSE)
  expect_identical(load_appusage_data_object(fp), load_appusage_data_object(pp))
  plan <- plan_appusage_workflow(project_dir = fresh$project_dir)
  expect_true(all(plan$tasks$action %in% c("reuse", "disabled")))
})

test_that("fresh primary manifests resolve through the existing first-level linkage", {
  parent <- tempfile("category039-primary-"); dir.create(parent)
  source <- normalized_summary_path(test_path("fixtures", "line_sample.txt"))
  manifest <- data.frame(source_id = "s", person_id = "p", observation_id = "o",
    selected_primary = TRUE, source_file = source)
  result <- run_appusage_workflow(source, project_dir = parent,
    config = appusage_config(category = list(enabled = TRUE, manifest = manifest),
      execution = list(progress = FALSE)))
  profile <- readRDS(file.path(parent, "appusage_category_profile.rds"))$profile
  expect_identical(profile$manifest$source_id, "s")
  expect_identical(profile$manifest$rda_path, appusage_summary_proc2_paths(result$second_level))
  expect_true(all(plan_appusage_workflow(project_dir = parent)$tasks$action %in% c("reuse", "disabled")))
})

test_that("category worker callbacks do not serialize the caller population", {
  expect_identical(environment(appusage_category_cache_worker), asNamespace("appusageR"))
  # pkgload's development namespace itself is serialized; installed namespaces
  # are referenced by name. Neither may capture a 152 MB caller profile.
  installed <- file.exists(file.path(getNamespaceInfo(asNamespace("appusageR"), "path"), "Meta", "package.rds"))
  expect_lt(length(serialize(appusage_category_cache_worker, NULL)), if (installed) 5000L else 1000000L)
})

test_that("category transactions recover without scanning flat directories", {
  parent <- tempfile("category039-journal-"); dir.create(parent)
  first <- read_appusage_batch(test_path("fixtures", "line_sample.txt"), ids = "1",
    output_dir = parent, project_name = "Journal", project_id = "039", progress = FALSE)
  second <- write_second_level_batch(first, progress = FALSE)
  rda <- second$second_level_data_file[[1]]; json <- sub("[.]rda$", ".json", rda)
  old <- tools::md5sum(c(rda, json)); paths <- appusage_category_transaction_paths(rda, json)
  record <- list(rda = rda, json = json, old_artifact = appusage_artifact_signature(rda),
    old_json_md5 = unname(appusage_file_md5(json)), new_artifact = list(size = 1, md5 = "invalid"), new_contract = list(changed = TRUE))
  saveRDS(record, paths$journal)
  file.rename(rda, paths$backup_rda); file.rename(json, paths$backup_json)
  writeBin(charToRaw("partial publication"), rda)
  appusage_category_recover_pair(rda, json)
  expect_identical(tools::md5sum(names(old)), old)
  expect_false(any(file.exists(unlist(paths))))
  local_mocked_bindings(appusage_second_level_owned_artifacts = function(...) stop("Unexpected directory scan"))
  expect_identical(update_app_categories_cache(rda, progress = FALSE)$action, "run")
  expect_identical(update_app_categories_cache(rda, progress = FALSE)$action, "reuse")
})

test_that("eight category workers produce the serial result and reuse unchanged pairs", {
  skip_if(parallel::detectCores() < 8L)
  parent <- tempfile("category039-workers-"); dir.create(parent)
  first <- read_appusage_batch(test_path("fixtures", "line_sample.txt"), ids = "1",
    output_dir = parent, project_name = "Workers", project_id = "039", progress = FALSE)
  second <- write_second_level_batch(first, progress = FALSE)
  path <- second$second_level_data_file[[1]]; json <- second_level_metadata_path(path)
  dirs <- file.path(parent, paste0("copy", seq_len(64)))
  for (dir in dirs) {
    dir.create(dir); file.copy(c(path, json), dir)
  }
  paths <- file.path(dirs, basename(path))
  receipt <- update_app_categories_cache(paths, category039_dictionary(), parallel = TRUE,
    workers = 8L, checkpoint_every = 64L, progress = FALSE)
  expect_equal(length(unique(receipt$worker_pid)), 8L)
  expect_true(all(receipt$worker_arch == R.version$arch))
  serial <- update_app_categories_cache(path, category039_dictionary(), progress = FALSE)
  expected <- load_appusage_data_object(path)
  expect_true(all(vapply(paths, function(p) identical(load_appusage_data_object(p), expected), logical(1))))
  before <- tools::md5sum(paths)
  reused <- update_app_categories_cache(paths, category039_dictionary(), progress = FALSE)
  expect_true(all(reused$action == "reuse"))
  expect_identical(tools::md5sum(paths), before)
})

test_that("category JSON compaction preserves parsed values and array/object shapes", {
  original <- list(arrays = list(single = list(1L), many = as.list(seq_len(10000L)),
      decimal = list(1L, 1e-7, -123.4567891234567), flags = list(TRUE, FALSE),
      names = list("微信", "quote\"", "\\", "line\n"), nullable = list(1L, NULL),
      empty = list()), scalar = 2L, absent = NULL,
    object = list(named = list(value = 1L)), singleton_object_array = list(list(value = 3L)))
  encode <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null", digits = NA)
  ready <- appusage_category_json_ready(original)
  expect_identical(jsonlite::fromJSON(encode(ready), simplifyVector = FALSE),
    jsonlite::fromJSON(encode(original), simplifyVector = FALSE))
  expect_s3_class(ready$arrays$many, "AsIs")
  expect_length(ready$arrays$many, 10000L)
  expect_identical(ready$arrays$nullable, original$arrays$nullable)
  expect_identical(ready$absent, NULL)
  expect_identical(ready$object, original$object)
  expect_match(encode(ready), '"single":\\[1\\]')
  expect_match(encode(ready), '"absent":null')
})
