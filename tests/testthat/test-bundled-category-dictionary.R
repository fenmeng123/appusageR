test_that("bundled dictionary exposes its approved version and raw identity contract", {
  env <- new.env(parent = emptyenv())
  utils::data("app_category_dictionary", package = "appusageR", envir = env)
  dict <- env$app_category_dictionary
  provenance <- attr(dict, "dictionary_provenance")
  expect_identical(class(dict), "data.frame")
  expect_true(all(vapply(dict, is.character, logical(1))))
  expect_identical(provenance$dictionary_version, "v2.0.0")
  expect_identical(provenance$parent_mapping_version, "v1.0.0")
  expect_identical(provenance$legacy_dictionary_version, "v261019")
  expect_identical(provenance$legacy_parent_mapping_version, "v261013")
  expect_identical(provenance$redistribution_license, "GPL-3")
  expect_identical(nrow(dict), 28088L)
  expect_identical(names(dict), c("App_Name", "App_Name_Repaired", "App_UUID",
                                 "Level_1_Category", "Level_2_Category"))
  expect_identical(sum(is.na(dict$App_Name_Repaired[seq_len(11849L)])), 10994L)
  expect_identical(dict$App_UUID[[11844]], "com.perol.\u5b89    \u5168    \u670d    \u52a1    ")
})

test_that("bundled extension keeps provisional residuals separate from functional claims", {
  env <- new.env(parent = emptyenv())
  utils::data("app_category_dictionary", package = "appusageR", envir = env)
  dict <- env$app_category_dictionary
  provenance <- attr(dict, "dictionary_provenance")
  expect_identical(provenance$approved_high_confidence_identities, 726L)
  expect_identical(provenance$approved_medium_confidence_identities, 1199L)
  expect_identical(provenance$user_assigned_residual_identities, 12193L)
  expect_identical(provenance$functional_level_1_count, 14L)
  expect_identical(provenance$functional_level_2_count, 31L)
  residual <- dict$Level_1_Category == "Others"
  expect_identical(sum(residual), 13594L)
  expect_true(all(dict$Level_2_Category[residual] == "Unclassified"))
  app <- dict[which(residual)[[1L]], , drop = FALSE]
  out <- add_app_categories(data.frame(package_name = app$App_UUID,
                                     app_name = app$App_Name), dict)
  expect_identical(out$Level_1_Category, "Others")
  expect_identical(out$Level_2_Category, "Unclassified")
})

test_that("bundled labels implement author overrides and have consistent parents", {
  env <- new.env(parent = emptyenv())
  utils::data("app_category_dictionary", package = "appusageR", envir = env)
  dict <- env$app_category_dictionary
  prepared <- as_app_category_dictionary(dict)
  expect_identical(attr(prepared, "app_category_diagnostics")$n_app_uuid_conflicts, 0L)
  parents <- split(dict$Level_1_Category, dict$Level_2_Category)
  expect_true(all(vapply(parents, function(x) length(unique(x)) == 1L, logical(1))))
  out <- add_app_categories(data.frame(
    package_name = c("com.vivo.wallet", "com.xyn.wskai", "com.vortex"),
    app_name = "synthetic name"
  ), dict)
  expect_identical(out$app_category_match_status, rep("matched", 3))
  expect_identical(out$Level_1_Category, c("Utilities and Tools", "Video Gaming", "Utilities and Tools"))
  expect_identical(out$Level_2_Category, c("System Utilities", "Gaming Support", "System Utilities"))
})
