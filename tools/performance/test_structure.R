Sys.setenv(R_USER_CACHE_DIR = "E:/mSens_AppUsage/.r-cache", XDG_CACHE_HOME = "E:/mSens_AppUsage/.cache")
devtools::document()
devtools::test(filter = "parse|detect|preflight|mixed-content|api-wrappers", reporter = "summary")
