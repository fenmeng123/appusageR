appusage_category_profile_dependencies <- function(manifest) {
  paths <- unique(normalized_summary_path(manifest$rda_path))
  lapply(paths, function(path) {
    m <- appusage_read_json_safely(second_level_metadata_path(path))
    list(path = path, identity = m$identity, research = m$module_state$research_data,
      qc = m$module_state$qc, artifact = m$module_state$artifact)
  })
}

appusage_workflow_category_profile <- function(project_dir, settings, summary = NULL,
                                                build = FALSE) {
  if (!is.null(settings$profile)) return(settings$profile)
  if (is.null(settings$manifest)) return(NULL)
  dictionary <- as_app_category_dictionary(settings$dictionary)
  manifest <- settings$manifest
  if (!"rda_path" %in% names(manifest)) {
    if (is.null(summary)) summary <- appusage_read_summary_csv(file.path(project_dir,
      "analytic_summary_table_proclevel-2.csv"))
    if (!nrow(summary)) return(NULL)
    if (!"source_file" %in% names(manifest)) cli::cli_abort("Category manifest needs source_file or rda_path.")
    first <- appusage_read_summary_csv(file.path(project_dir,
      "analytic_summary_table_proclevel-1.csv"))
    resolved <- appusage_manifest_with_proc2_paths(manifest, first, summary, project_dir)
    manifest$rda_path <- resolved$second_level_rda
  }
  if (anyNA(manifest$rda_path) || any(!nzchar(manifest$rda_path)))
    cli::cli_abort("Category manifest source files must resolve to completed proc-2 paths.")
  manifest <- appusage_category_manifest(manifest)
  file <- file.path(project_dir, "appusage_category_profile.rds")
  requested <- list(corpus = appusage_category_fingerprint(manifest),
    dictionary = attr(dictionary, "dictionary_content_hash"),
    policy = appusage_object_fingerprint(unclass(settings$policy)))
  dependencies <- appusage_category_profile_dependencies(manifest)
  prior <- if (file.exists(file)) tryCatch(readRDS(file), error = function(e) NULL) else NULL
  if (!is.null(prior) && identical(prior$requested, requested) &&
      identical(prior$dependencies, dependencies)) {
    appusage_validate_category_profile(prior$profile, dictionary, settings$policy)
    return(prior$profile)
  }
  if (!build) return(NULL)
  profile <- profile_uncoded_apps(manifest, dictionary, settings$policy)
  saveRDS(list(requested = requested, dependencies = dependencies,
    manifest = manifest, profile = profile), file)
  profile
}

appusage_finish_workflow_category_profile <- function(project_dir) {
  file <- file.path(project_dir, "appusage_category_profile.rds")
  if (!file.exists(file)) return(invisible(NULL))
  prior <- readRDS(file)
  prior$dependencies <- appusage_category_profile_dependencies(prior$manifest)
  saveRDS(prior, file)
  invisible(NULL)
}
