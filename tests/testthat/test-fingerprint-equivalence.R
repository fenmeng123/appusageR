legacy_object_fingerprint <- function(x) {
  appusage_stable_text_md5(appusage_text_paste(
    utils::capture.output(dput(appusage_canonical_object(x))), collapse = "\n"))
}

legacy_function_fingerprint <- function(function_names) {
  namespace <- asNamespace("appusageR")
  pending <- unique(function_names)
  expanded <- character()
  while (length(pending)) {
    name <- pending[[1L]]
    pending <- pending[-1L]
    if (name %in% expanded) next
    expanded <- c(expanded, name)
    fun <- get0(name, envir = namespace, inherits = FALSE)
    if (!is.function(fun)) next
    symbols <- all.names(body(fun), functions = TRUE, unique = TRUE)
    local <- symbols[vapply(symbols, function(symbol)
      is.function(get0(symbol, envir = namespace, inherits = FALSE)), logical(1))]
    pending <- unique(c(pending, setdiff(local, expanded)))
  }
  text <- unlist(lapply(sort(unique(expanded)), function(name) {
    fun <- get0(name, envir = namespace, inherits = FALSE)
    if (!is.function(fun)) return(c(name, "<unavailable>"))
    c(appusage_text_paste0("function=", name),
      appusage_text_paste(utils::capture.output(dput(formals(fun))), collapse = "\n"),
      appusage_text_paste(deparse(body(fun), width.cutoff = 500L), collapse = "\n"))
  }), use.names = FALSE)
  appusage_stable_text_md5(text)
}

test_that("file-backed fingerprints retain the original representation", {
  objects <- list(NULL, list(z = 1:4, a = c(NA, Inf, -Inf, NaN)),
    data.frame(text = c("\u4e2d\u6587", "caf\u00e9", "line\nquote\"", NA),
      value = c(1/3, 1e-100, NA, 1e100)),
    list(date = as.Date("2024-01-02"), time = as.POSIXct("2024-01-02", tz = "UTC")),
    appusage_default_dictionary())
  for (object in objects)
    expect_identical(appusage_object_fingerprint(object), legacy_object_fingerprint(object))
})

test_that("cached function text verifies definitions and traverses changed dependencies", {
  roots <- c("appusage_source_preflight", "make_second_level_appusage",
    "qc_appusage_anomalies", "appusage_output_schema_version", "missing_test_function")
  expect_identical(appusage_function_fingerprint(roots), legacy_function_fingerprint(roots))
  expect_identical(appusage_function_fingerprint(roots), legacy_function_fingerprint(roots))
  before <- appusage_function_fingerprint("appusage_output_schema_version")
  local_mocked_bindings(appusage_output_schema_version = function(x = 1L) appusage_pair_text(x))
  after <- appusage_function_fingerprint("appusage_output_schema_version")
  expect_false(identical(before, after))
  expect_identical(after, legacy_function_fingerprint("appusage_output_schema_version"))
  local_mocked_bindings(appusage_pair_text = function(...) "changed dependency")
  changed <- appusage_function_fingerprint("appusage_output_schema_version")
  expect_false(identical(after, changed))
  expect_identical(changed, legacy_function_fingerprint("appusage_output_schema_version"))
})

test_that("a previously non-function namespace binding invalidates a cached graph", {
  root <- ".appusage_category_index_cache"
  before <- appusage_function_fingerprint(root)
  expect_identical(before, legacy_function_fingerprint(root))
  local_mocked_bindings(.appusage_category_index_cache = function() appusage_output_schema_version())
  after <- appusage_function_fingerprint(root)
  expect_false(identical(before, after))
  expect_identical(after, legacy_function_fingerprint(root))
})
