test_that("bundled datasets can be loaded", {
  skip_if_not_installed("plsdepot")
  for (ds in c("carscomplete", "carsmissing", "cornell", "linnerud", "ropes", "vehicles")) {
    expect_silent(data(list = ds, package = "plsdepot", envir = new.env()))
  }
})
