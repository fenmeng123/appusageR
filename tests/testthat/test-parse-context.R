test_that("ragged parsing retains row identity without a width-sized allocation", {
  lines <- c(rep("a,b", 100L), paste(rep("wide", 10000L),collapse=","), "", ",,")
  context <- appusage_parse_context(lines)
  expect_identical(nrow(context),length(lines))
  expect_identical(ncol(context),10000L)
  expect_identical(length(context$store$values),sum(lengths(strsplit(lines,",",fixed=TRUE))))
  expect_equal(appusage_context_column(context,2L)[1:3],rep("b",3L))
  expect_true(is.na(appusage_context_column(context,9999L)[[1L]]))
  view <- context[c(101L,1L),,drop=FALSE]
  expect_identical(view$store,context$store)
  expect_identical(appusage_context_column(view,1L),c("wide","a"))
})

test_that("explicit Chinese source encodings are decoded once across public entries", {
  for (type in c("line", "meta", "day", "app")) {
    fixture <- test_path("fixtures", paste0(type, "_sample.txt"))
    source <- paste(readLines(fixture, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    for (encoding in c("GBK", "GB18030", "CP936")) {
      path <- tempfile(fileext = ".txt")
      writeBin(stringi::stri_encode(source, from = "UTF-8", to = encoding, to_raw = TRUE)[[1L]], path)
      expected <- get(paste0("parse_", type))(path, participant_id = "encoding-test", encoding = encoding)
      direct <- run_first_level_appusage(path, participant_id = "encoding-test", encoding = encoding)
      decoded <- read_appusage_text(path, encoding = encoding)
      staged <- run_first_level_appusage(decoded, participant_id = "encoding-test", encoding = encoding)
      expect_identical(direct$status, "success", info = paste(type, encoding))
      expect_identical(staged$status, "success", info = paste(type, encoding, "staged"))
      expect_identical(direct$parsed, expected)
      expect_identical(staged$parsed, expected)
      unlink(path)
    }
  }
})

test_that("header matching is shared by context views and private state stays private", {
  context <- appusage_parse_context(c("a,b","header,start","c,d"))
  calls <- 0L
  local_mocked_bindings(appusage_text_detect=function(string,pattern) {
    calls <<- calls+1L; stringi::stri_detect_regex(string,pattern)
  })
  expect_identical(appusage_context_hits(context,"header"),c(FALSE,TRUE,FALSE))
  expect_identical(appusage_context_hits(context[2:3,,drop=FALSE],"header"),c(TRUE,FALSE))
  expect_identical(calls,1L)
  contains_private <- function(x) {
    if(is.environment(x)||inherits(x,'appusage_parse_context')) return(TRUE)
    if(is.list(x)&&any(vapply(x,contains_private,logical(1)))) return(TRUE)
    a<-attributes(x)
    !is.null(a)&&any(vapply(a,contains_private,logical(1)))
  }
  for(type in c('line','meta','day','app')) {
    parsed<-get(paste0('parse_',type))(test_path('fixtures',paste0(type,'_sample.txt')))
    expect_false(contains_private(parsed))
    input<-if(type=='meta') parsed else setNames(list(parsed),type)
    expect_false(contains_private(make_second_level_appusage(input)))
  }
})
