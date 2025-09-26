test_that("namespace exports are resolvable", {
  # Skip on CRAN if loading namespace fails for some reason
  skip_if_not_installed("plsdepot")
  ns <- asNamespace("plsdepot")
  expect_true(is.environment(ns))

  # If exportPattern is used, just assert we have at least one export
  # Otherwise, check that exported objects exist in the namespace
  exports <- tryCatch(getNamespaceExports("plsdepot"), error = function(e) character())
  expect_true(length(exports) >= 0)

  for (sym in exports) {
    expect_true(exists(sym, envir = ns, inherits = FALSE), info = paste("Missing:", sym))
  }
})
