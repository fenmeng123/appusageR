#' Update category annotations in existing proc-2 RDA/JSON pairs
#'
#' Accepts project caches or flat copied files. No parsing, scientific processing,
#' QC or questionnaire matching is invoked. Failed updates stop the batch.
#' @param manifest Character vector of RDA paths, or data frame with rda_path and
#'   optional json_path. With a profile, source_id must identify a frozen source.
#'   JSON must be beside the corresponding RDA.
#' @param dictionary NULL uses the bundled dictionary.
#' @param policy Classification policy.
#' @param profile Optional frozen unknown-app profile.
#' @param overwrite Replace existing category labels when TRUE.
#' @param resume Reuse intact pairs with the same category contract.
#' @param parallel,workers Enable file workers; resource failures fall back to fewer workers.
#' @param checkpoint_every Number of pairs per checkpoint.
#' @param checkpoint_file Optional RDS receipt file, separate from scientific caches.
#' @param progress Report bounded checkpoint progress.
#' @return A data frame of actions, worker identities and category counts.
#' @export
update_app_categories_cache <- function(manifest, dictionary = NULL,
                                        policy = appusage_category_policy(), profile = NULL,
                                        overwrite = TRUE, resume = TRUE,
                                        parallel = FALSE, workers = 8L,
                                        checkpoint_every = 100L, checkpoint_file = NULL,
                                        progress = TRUE) {
  if (is.character(manifest)) manifest <- data.frame(rda_path = manifest, stringsAsFactors = FALSE)
  if (!is.data.frame(manifest) || !"rda_path" %in% names(manifest)) cli::cli_abort("Cache updates require rda_path.")
  manifest <- as.data.frame(manifest, stringsAsFactors = FALSE)
  if (!"json_path" %in% names(manifest)) manifest$json_path <- sub("[.]rda$", ".json", manifest$rda_path)
  if (anyNA(manifest$rda_path) || anyNA(manifest$json_path) ||
      anyDuplicated(tolower(normalized_summary_path(manifest$rda_path))))
    cli::cli_abort("Cache RDA paths must be nonmissing and unique.")
  if (!identical(normalized_summary_path(manifest$json_path),
      normalized_summary_path(sub("[.]rda$", ".json", manifest$rda_path))))
    cli::cli_abort("Each JSON must be beside its RDA with the same basename.")
  policy <- appusage_validate_category_policy(policy)
  dictionary <- as_app_category_dictionary(dictionary)
  if (!is.null(profile)) appusage_validate_category_profile(profile, dictionary, policy)
  if (!is.null(profile)) {
    if (!"source_id" %in% names(manifest) || anyNA(manifest$source_id) ||
        any(!manifest$source_id %in% profile$source_ids))
      cli::cli_abort("Profile cache updates require source_id membership in the frozen corpus.")
  }
  if (length(checkpoint_every) != 1L || is.na(checkpoint_every) || checkpoint_every < 1 ||
      checkpoint_every %% 1 != 0) cli::cli_abort("checkpoint_every must be a positive integer.")
  workers <- validate_parallel_settings(parallel, if (parallel) workers else 1L)
  expected <- appusage_category_contract(dictionary, overwrite, policy, profile)
  slim <- if (is.null(profile)) NULL else structure(profile[c("apps", "contract", "fingerprint", "source_status", "source_ids")], class = class(profile))
  context <- list(manifest = manifest, dictionary = dictionary, policy = policy,
    profile = slim, overwrite = overwrite, resume = resume, expected = expected)
  cluster <- NULL
  on.exit(if (!is.null(cluster)) appusage_stop_cluster_safely(cluster), add = TRUE)
  start_cluster <- function(n) {
    cl <- parallel::makePSOCKcluster(n)
    ok <- FALSE
    on.exit(if (!ok) appusage_stop_cluster_safely(cl), add = TRUE)
    package_root <- getNamespaceInfo(asNamespace("appusageR"), "path")
    startup <- function(root, libs) {
      Sys.setenv(LANG = "Chinese_China.utf8", LC_ALL = "Chinese_China.utf8")
      .libPaths(libs)
      if (file.exists(file.path(root, "Meta", "package.rds"))) {
        loadNamespace("appusageR", lib.loc = dirname(root))
      } else {
        pkgload::load_all(root, quiet = TRUE, helpers = FALSE)
      }
      NULL
    }
    # Avoid serializing the updater frame (full population profile and receipts).
    environment(startup) <- baseenv()
    parallel::clusterCall(cl, startup, package_root, .libPaths())
    parallel::clusterExport(cl, "context", envir = environment())
    ok <- TRUE
    cl
  }
  run_one <- function(i) {
    c <- context
    appusage_category_source_matches_profile(c$manifest[i, , drop = FALSE], c$profile)
    appusage_update_category_pair(c$manifest$rda_path[[i]], c$manifest$json_path[[i]],
      c$dictionary, c$policy, c$profile, c$overwrite, c$resume, c$expected)
  }
  results <- vector("list", nrow(manifest))
  starts <- if (nrow(manifest)) seq.int(1L, nrow(manifest), by = as.integer(checkpoint_every)) else integer()
  for (first in starts) {
    indices <- seq.int(first, min(nrow(manifest), first + checkpoint_every - 1L))
    repeat {
      chunk <- tryCatch({
        if (workers == 1L) lapply(indices, run_one) else {
          if (is.null(cluster)) cluster <- start_cluster(workers)
          parallel::parLapplyLB(cluster, indices, appusage_category_cache_worker)
        }
      }, error = function(e) e)
      if (!inherits(chunk, "error")) break
      resource <- appusage_is_cluster_transport_error(chunk) || grepl(
        "cannot allocate|memory exhausted|out of memory", conditionMessage(chunk), ignore.case = TRUE)
      if (!resource || workers == 1L) stop(chunk)
      if (!is.null(cluster)) appusage_stop_cluster_safely(cluster)
      cluster <- NULL
      workers <- max(1L, floor(workers / 2L))
      context$resume <- TRUE
      cli::cli_warn("Category resource fallback to {workers} worker(s); completed intact pairs are reused.")
    }
    results[indices] <- chunk
    if (!is.null(checkpoint_file)) {
      dir.create(dirname(checkpoint_file), recursive = TRUE, showWarnings = FALSE)
      checkpoint <- list(contract = expected, completed = do.call(rbind, results[seq_len(max(indices))]))
      temporary <- paste0(checkpoint_file, ".tmp")
      saveRDS(checkpoint, temporary)
      if (file.exists(checkpoint_file)) unlink(checkpoint_file)
      if (!file.rename(temporary, checkpoint_file)) cli::cli_abort("Could not publish category checkpoint.")
    }
    if (progress) message(sprintf("Categories: %d/%d pairs (%d workers)", max(indices), nrow(manifest), workers))
  }
  if (!length(results)) return(data.frame(rda_path = character(), action = character()))
  do.call(rbind, results)
}

appusage_category_cache_worker <- function(i) {
  context <- get("context", envir = .GlobalEnv)
  appusage_category_source_matches_profile(context$manifest[i, , drop = FALSE], context$profile)
  appusage_update_category_pair(context$manifest$rda_path[[i]], context$manifest$json_path[[i]],
    context$dictionary, context$policy, context$profile, context$overwrite, context$resume, context$expected)
}

appusage_category_source_matches_profile <- function(row, profile) {
  if (is.null(profile) || !"rda_md5" %in% names(profile$source_status)) return(invisible(NULL))
  appusage_category_recover_pair(row$rda_path[[1]], row$json_path[[1]])
  recorded <- profile$source_status[match(row$source_id, profile$source_status$source_id), , drop = FALSE]
  m <- appusage_read_json_safely(row$json_path[[1]])
  signed_by_profile <- identical(m$module_state$category$profile, profile$fingerprint)
  if (!signed_by_profile && !appusage_contract_equal(m$module_state$artifact,
      list(size = recorded$rda_size[[1]], md5 = recorded$rda_md5[[1]])))
    cli::cli_abort("Frozen profile source artifact changed; reprofile before updating categories.")
  invisible(NULL)
}

appusage_update_category_pair <- function(rda, json, dictionary, policy, profile,
                                          overwrite, resume, expected) {
  started <- proc.time()[[3]]
  appusage_category_recover_pair(rda, json)
  if (!file.exists(rda) || !file.exists(json)) cli::cli_abort("Category update requires both RDA and JSON.")
  metadata <- appusage_read_json(json, simplifyVector = FALSE)
  if (!appusage_artifact_valid(rda, metadata$module_state$artifact))
    cli::cli_abort("Category update requires an intact verified research artifact: {basename(rda)}")
  action <- "reuse"
  if (!resume || !appusage_contract_equal(metadata$module_state$category, expected)) {
    old <- load_appusage_data_object(rda)
    fresh <- appusage_add_categories_prepared(old, dictionary, overwrite, policy, profile)
    if (!identical(appusage_without_categories(old), appusage_without_categories(fresh)))
      cli::cli_abort("Category update attempted to change non-category data.")
    summary <- attr(fresh, "app_category_summary", exact = TRUE)
    write_app_category_second_level_pair(fresh, dirname(rda), rda, summary,
      dictionary, overwrite, policy, profile, expected, metadata, metadata_file = json)
    metadata <- appusage_read_json(json, simplifyVector = FALSE)
    action <- "run"
  }
  counts <- metadata$category_dictionary
  data.frame(rda_path = rda, action = action, status = "success", worker_pid = Sys.getpid(),
    worker_arch = R.version$arch, elapsed_seconds = proc.time()[[3]] - started,
    n_category_rows = counts$n_category_rows %||% 0L,
    n_category_matched_rows = counts$n_category_matched_rows %||% 0L,
    n_category_auto_other_rows = counts$n_category_auto_other_rows %||% 0L,
    n_category_pending_rows = counts$n_category_pending_rows %||% 0L,
    n_category_conflict_rows = counts$n_category_conflict_rows %||% 0L,
    n_category_not_app_rows = counts$n_category_not_app_rows %||% 0L,
    n_category_unique_apps = counts$n_category_unique_apps %||% 0L,
    n_category_matched_apps = counts$n_category_matched_apps %||% 0L,
    n_category_unmatched_apps = counts$n_category_unmatched_apps %||% 0L,
    stringsAsFactors = FALSE)
}
