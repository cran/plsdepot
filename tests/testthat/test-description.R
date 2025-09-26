test_that("DESCRIPTION has key fields", {
  d <- utils::packageDescription("plsdepot")
  expect_type(d, "list")
  expect_true(nzchar(d$Package))
  expect_true(nzchar(d$Version))
  expect_true(nzchar(d$Title))
})
