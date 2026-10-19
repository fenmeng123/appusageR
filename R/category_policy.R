#' Configure optional app classification and manual-review priorities
#'
#' These thresholds prioritize dictionary maintenance; they never exclude users
#' or change scientific QC. Residual labels are operational, not manual codes.
#' @param share_threshold Maximum primary-file foreground share triggering review.
#' @param prevalence_threshold Positive-foreground person prevalence triggering review.
#' @param auto_other Assign the operational residual pair to measured low-priority unknowns.
#' @param other_l1,other_l2 Labels used only for operational residual classification.
#' @return A versioned `appusage_category_policy` list.
#' @examples
#' policy <- appusage_category_policy()
#' policy$share_threshold
#' policy$prevalence_threshold
#' # Keep unknown applications pending rather than assigning residual labels.
#' appusage_category_policy(auto_other = FALSE)
#' @export
appusage_category_policy <- function(share_threshold = 0.05,
                                     prevalence_threshold = 0.01,
                                     auto_other = TRUE,
                                     other_l1 = "Others", other_l2 = "Unclassified") {
  for (v in list(share_threshold, prevalence_threshold)) {
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || !is.finite(v) || v < 0 || v > 1)
      cli::cli_abort("Category thresholds must be finite scalar proportions in [0, 1].")
  }
  if (!is.logical(auto_other) || length(auto_other) != 1L || is.na(auto_other))
    cli::cli_abort("`auto_other` must be TRUE or FALSE.")
  if (any(vapply(list(other_l1, other_l2), function(v)
      !is.character(v) || length(v) != 1L || is.na(v) || !nzchar(trimws(v)), logical(1))))
    cli::cli_abort("Residual labels must be nonempty character scalars.")
  structure(list(version = "category-policy-1", share_threshold = share_threshold,
    prevalence_threshold = prevalence_threshold, auto_other = auto_other,
    other_l1 = other_l1, other_l2 = other_l2, share_aggregation = "person_max_primary",
    prevalence_presence = "positive_foreground", threshold_operator = "OR",
    metric_source = "saved_canonical_foreground", missing_metrics = "pending"),
    class = c("appusage_category_policy", "list"))
}

appusage_validate_category_policy <- function(policy) {
  if (is.null(policy)) return(appusage_category_policy())
  if (!inherits(policy, "appusage_category_policy")) cli::cli_abort("Use `appusage_category_policy()`.")
  fresh <- appusage_category_policy(policy$share_threshold, policy$prevalence_threshold,
    policy$auto_other, policy$other_l1, policy$other_l2)
  if (!identical(fresh, policy)) cli::cli_abort("Category policy was modified outside its constructor.")
  policy
}

appusage_default_dictionary <- function() {
  env <- new.env(parent = emptyenv())
  utils::data("app_category_dictionary", package = "appusageR", envir = env)
  env$app_category_dictionary
}

appusage_category_key <- function(package, name) {
  package <- standardize_package_name(package)
  name <- category_name_key(name)
  key <- ifelse(!is.na(package), paste0("pkg:", package), paste0("name:", name))
  key[is.na(package) & is.na(name)] <- NA_character_
  key
}

appusage_resolve_category_keys <- function(package, name, dictionary, is_all = NULL) {
  package <- standardize_package_name(package)
  name <- category_name_key(name)
  n <- length(package)
  out <- data.frame(Level_1_Category = rep(NA_character_, n),
    Level_2_Category = rep(NA_character_, n), app_category_match_method = rep(NA_character_, n),
    app_category_match_key = rep(NA_character_, n), app_category_match_status = rep("unmatched", n),
    app_category_disposition = rep("needs_profile", n),
    app_category_name_disagreement = rep(FALSE, n), stringsAsFactors = FALSE)
  look <- attr(dictionary, "app_category_lookups", exact = TRUE)
  out <- apply_category_match(out, package, look$app_uuid, "app_uuid", rep(TRUE, n))
  # Only conflicting APKs may be narrowed by names, never by another APK.
  conflicted <- which(out$app_category_match_status == "conflict")
  if (length(conflicted)) {
    candidates <- attr(dictionary, "app_category_candidates", exact = TRUE)
    for (i in conflicted) {
      rows <- candidates[[package[[i]]]]
      if (is.null(rows) || is.na(name[[i]])) next
      narrowed <- rows[!is.na(rows$.app_name_repaired_key) & rows$.app_name_repaired_key == name[[i]], , drop = FALSE]
      method <- "app_uuid_name_repaired"
      if (!nrow(narrowed)) {
        narrowed <- rows[!is.na(rows$.app_name_key) & rows$.app_name_key == name[[i]], , drop = FALSE]
        method <- "app_uuid_name"
      }
      labels <- unique(narrowed[c("Level_1_Category", "Level_2_Category")])
      if (nrow(labels) == 1L) {
        out$Level_1_Category[[i]] <- labels$Level_1_Category[[1]]
        out$Level_2_Category[[i]] <- labels$Level_2_Category[[1]]
        out$app_category_match_status[[i]] <- "matched"
        out$app_category_match_method[[i]] <- method
      }
    }
  }
  named <- apply_category_match(out, name, look$app_name_repaired, "app_name_repaired",
    out$app_category_match_status == "unmatched")
  named <- apply_category_match(named, name, look$app_name, "app_name",
    named$app_category_match_status == "unmatched")
  # Record disagreements without overriding an explicit package decision.
  name_only <- apply_category_match(out, name, look$app_name_repaired, "app_name_repaired", rep(TRUE, n))
  need_original <- is.na(name) | !name %in% c(look$app_name_repaired$matches$key, look$app_name_repaired$conflicts$key)
  name_only <- apply_category_match(name_only, name, look$app_name, "app_name", need_original)
  explicit <- out$app_category_match_status == "matched"
  different <- name_only$app_category_match_status == "matched" &
    (name_only$Level_1_Category != out$Level_1_Category | name_only$Level_2_Category != out$Level_2_Category)
  named$app_category_name_disagreement <- explicit & (!is.na(different) & different |
    name_only$app_category_match_status == "conflict")
  named$app_category_disposition[named$app_category_match_status == "matched"] <- "dictionary_matched"
  named$app_category_disposition[named$app_category_match_status == "conflict"] <- "needs_review_conflict"
  missing <- is.na(package) & is.na(name)
  named$app_category_disposition[missing] <- "missing_identity"
  total <- package %in% "ALL"
  if (!is.null(is_all)) total <- total | is_all %in% TRUE
  named[total, c("Level_1_Category", "Level_2_Category", "app_category_match_method", "app_category_match_key")] <- NA_character_
  named$app_category_match_status[total] <- "not_app"
  named$app_category_disposition[total] <- "not_app"
  named$app_category_name_disagreement[total] <- FALSE
  named
}

appusage_category_fields <- function() c("Level_1_Category", "Level_2_Category",
  "app_category_match_method", "app_category_match_key", "app_category_match_status",
  "app_category_disposition", "app_category_name_disagreement")

appusage_without_categories <- function(data) {
  strip <- function(x) {
    if (is.data.frame(x)) x <- x[, setdiff(names(x), appusage_category_fields()), drop = FALSE]
    attr(x, "app_category_summary") <- NULL
    x
  }
  if (is.data.frame(data)) return(strip(data))
  out <- data
  for (nm in names(out)) if (is.data.frame(out[[nm]])) out[[nm]] <- strip(out[[nm]])
  attr(out, "app_category_summary") <- NULL
  out
}
