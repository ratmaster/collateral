context("Global options")

test_that("options are loaded", {
  expect_is(getOption("collateralopts"), "list")
  expect_gt(length(getOption("collateralopts")), 0)
})

test_that("options can be written and read", {
  from_here(title = "mytitle")
  expect_identical(getOption("collateralopts")$title, "mytitle")
})