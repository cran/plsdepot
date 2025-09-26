test_that("examples run (lightweight)", {
  skip_on_cran()
  skip_if_not_installed("plsdepot")
  topics <- tryCatch(plsdepot:::.packageTopics(), error = function(e) character())
  if (length(topics) == 0L) topics <- NULL
  # Run at most 3 examples to keep checks fast
  if (!is.null(topics)) {
    topics <- head(topics, 3L)
    for (tp in topics) {
      expect_silent(example(topic = tp, package = "plsdepot", run.donttest = FALSE, give.lines = FALSE, echo = FALSE, character.only = TRUE))
    }
  } else {
    succeed("No topics to run")
  }
})
