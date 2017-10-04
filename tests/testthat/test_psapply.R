context("psapply")

# test dataset
set.seed(1234)
x <- list()
for (i in 1:10) {
  x[[i]] <- runif(100)
}

test_that("psapply behaves like sapply", {
  expect_identical(psapply(x, mean),
                   sapply(x, mean))
  expect_identical(psapply(x, mean, trim = 0.1),
                   sapply(x, mean, trim = 0.1))
})