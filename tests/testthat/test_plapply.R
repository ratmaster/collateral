context("plapply")

# test dataset
set.seed(1234)
x <- list()
for (i in 1:10) {
  x[[i]] <- runif(100)
}

# test results
y1 <- lapply(x, mean)
y2 <- lapply(x, mean, trim = 0.1)

test_that("plapply computes correctly with one thread", {
  expect_identical(plapply(x, mean), y1)
  expect_identical(plapply(x, mean, trim = 0.1), y2)
})

test_that("plapply computes correctly with multiple threads", {
  skip_on_os("windows")
  skip_on_cran()
  expect_identical(plapply(x, mean,
                           threads = 3),
                   y1)
  expect_identical(plapply(x, mean, trim = 0.1,
                           threads = 3),
                   y2)
})

test_that("plapply simplifies return object", {
  expect_identical(plapply(x, mean,
                           simplify = TRUE),
                   sapply(x, mean))
})

test_that("plapply shows the progress with distinct threads", {
  skip_on_cran()
  expect_output(y <- plapply(x, mean,
                             title = "test",
                             progress = TRUE),
                "^test  \\.+ 100\\.00 %$")
  expect_identical(y, y1)
  skip_on_os("windows")
  expect_output(y <- plapply(x, mean,
                             title = "test",
                             progress = TRUE,
                             threads = 3),
                "^test  \\.+ 100\\.00 %$")
  expect_identical(y, y1)
})

test_that("plapply shows progress with times", {
  skip_on_cran()
  expect_output(y <- plapply(x, mean,
                             title = "test",
                             progress = TRUE,
                             eta = TRUE),
                "^test  .+\\d#")
  expect_identical(y, y1)
  expect_output(y <- plapply(x, mean,
                             title = "test",
                             progress = TRUE,
                             time = TRUE),
                "^test  .+\\ds$")
  expect_identical(y, y1)
  expect_output(y <- plapply(x, mean,
                             title = "test",
                             progress = TRUE,
                             eta = TRUE,
                             time = TRUE),
                "^test  .+\\d#")
  expect_identical(y, y1)
})

test_that("plapply handles function that returns NULL", {
  myfun <- function(x) {
  }
  expect_identical(plapply(1:10, myfun),
                   rep(list(NULL), 10))
  expect_identical(plapply(1:10, myfun,
                           threads = 3),
                   rep(list(NULL), 10))
})

test_that("plapply generates random numbers independently of threads", {
  skip_on_cran()
  set.seed(1234)
  y1 <- c(runif(1),
          plapply(rep(1, 10), runif,
                  simplify = TRUE),
          runif(1))
  set.seed(1234)
  y2 <- c(runif(1),
          plapply(rep(1, 10), runif,
                  threads = 2,
                  simplify = TRUE),
          runif(1))
  expect_identical(y1, y2)
  set.seed(1234)
  y3 <- c(runif(1),
          plapply(rep(1, 10), runif,
                  threads = 3,
                  simplify = TRUE),
          runif(1))
  expect_identical(y1, y3)
})

test_that("plapply generates correct random distributions", {
  set.seed(1234)
  y <- plapply(rep(1, 100), rnorm,
               simplify = TRUE)
  expect_gt(shapiro.test(y)$p.value, 0.5)
})

test_that("plapply generates same random numbers per iteration", {
  skip_on_cran()
  set.seed(1234)
  y1 <- c(runif(1),
          plapply(rep(1, 10), runif,
                  sameSeed = TRUE,
                  simplify = TRUE),
          runif(1))
  set.seed(1234)
  y2 <- c(runif(1),
          plapply(rep(1, 10), runif,
                  sameSeed = TRUE,
                  threads = 2,
                  simplify = TRUE),
          runif(1))
  expect_identical(y1, y2)
  set.seed(1234)
  y3 <- c(runif(1),
          plapply(rep(1, 10), runif,
                  sameSeed = TRUE,
                  threads = 3,
                  simplify = TRUE),
          runif(1))
  expect_identical(y1, y3)
})

test_that("plapply sets seed correctly", {
  skip_on_cran()
  set.seed(1234)
  preRandom.seed <- .GlobalEnv$.Random.seed
  set.seed(preRandom.seed[length(preRandom.seed)])
  y1 <- runif(1)
  set.seed(4567)
  y2 <- plapply(c(1, 1), runif,
                seed = 1234,
                sameSeed = TRUE,
                simplify = TRUE)
  expect_identical(c(y1, y1), y2)
})

test_that("plapply memorizes function calls", {
  y <- plapply(rep(1, 10), runif,
               simplify = TRUE)
  expect_equal(length(unique(y)), 10)
  y <- plapply(rep(1, 10), runif,
               memo = TRUE,
               simplify = TRUE)
  expect_equal(length(diff(y) == 0), 9)
})

test_that("plapply stops on error", {
  myfun <- function(x) {
    if (x == 5) {
      stop("ERROR")
    }
    return(x)
  }
  expect_error(plapply(1:10, myfun), "ERROR$")
  expect_error(plapply(1:10, myfun,
                       threads = 3),
               "ERROR$")
})

test_that("plapply collects error objects", {
  myfun <- function(x) {
    if (x == 5) {
      stop("ERROR")
    }
    cat(x)
    return(x)
  }
  expect_output(y1 <- plapply(1:10, myfun,
                              stopOnError = FALSE),
                "^1234678910$")
  expect_is(y1[[5]], "try-error")
  skip_on_cran()
  myfun <- function(x) {
    if (x == 5) {
      stop("ERROR")
    }
    return(x)
  }
  y2 <- plapply(1:10, myfun,
                stopOnError = FALSE,
                threads = 3)
  expect_identical(y1, y2)
})

test_that("plapply records and resumes the progress - serially", {
  # simple function with error
  flag <- FALSE
  myfun <- function(x) {
    if (x == 5 && flag == FALSE) {
      stop("ERROR")
    }
    cat(x)
    return(x)
  }
  set.seed(1234)
  expect_error(capture.output(
    plapply(1:10, myfun,
            resume = TRUE,
            simplify = TRUE)),
    "ERROR$")
  expect_true(dir.exists("tmp"))
  # fix error source outside of the function
  flag <- TRUE
  set.seed(1234)
  expect_output(y <- plapply(1:10, myfun,
                             resume = TRUE,
                             simplify = TRUE),
                "^5678910$")
  expect_identical(y, 1:10)
  # clean up
  unlink("tmp", recursive = TRUE)
})

test_that("plapply records and resumes the progress - parallely", {
  skip_on_cran()
  # simple function with error
  flag <- FALSE
  myfun <- function(x) {
    if (x == 5 && flag == FALSE) {
      stop("ERROR")
    }
    cat(x)
    return(x)
  }
  set.seed(1234)
  expect_error(plapply(1:10, myfun,
                       resume = TRUE,
                       threads = 3,
                       simplify = TRUE),
               "ERROR$")
  expect_true(dir.exists("tmp"))
  # fix error source outside of the function
  flag <- TRUE
  set.seed(1234)
  y1 <- plapply(1:10, myfun,
                resume = TRUE,
                threads = 3,
                simplify = TRUE)
  expect_identical(y1, 1:10)
  # place error again
  flag <- FALSE
  set.seed(1234)
  y2 <- plapply(1:10, myfun,
                resume = TRUE,
                threads = 3,
                simplify = TRUE)
  expect_identical(y1, y2)
  # clean up
  unlink("tmp", recursive = TRUE)
})

test_that("plapply records and resumes the progress - mixed", {
  # simple function with error
  flag <- FALSE
  myfun <- function(x) {
    if (x == 5 && flag == FALSE) {
      stop("ERROR")
    }
    cat(x)
    return(x)
  }
  set.seed(1234)
  expect_error(plapply(1:10, myfun,
                       resume = TRUE,
                       threads = 3,
                       simplify = TRUE),
               "ERROR$")
  expect_true(dir.exists("tmp"))
  # fix error source outside of the function
  flag <- TRUE
  set.seed(1234)
  expect_output(y <- plapply(1:10, myfun,
                             resume = TRUE,
                             simplify = TRUE),
                "^[^23]*5678910$")
  expect_identical(y, 1:10)
  # clean up
  unlink("tmp", recursive = TRUE)
})

test_that("plapply records and resumes the full progress", {
  # simple function with error
  from_here(title = "Test run")
  a <- 1
  myfun <- function(x) {
    return(a)
  }
  set.seed(1234)
  expect_output(y1 <- plapply(1:10, myfun,
                              progress = "batch",
                              resume = TRUE),
                "Test run  \\.+ 100\\.00 %")
  expect_true(dir.exists("tmp"))
  # fix error source outside of the function
  a <- 2
  set.seed(1234)
  expect_output(y2 <- plapply(1:10, myfun,
                              progress = "batch",
                              resume = TRUE),
                "Test run  \\.+ 100\\.00 %")
  expect_identical(y1, y2)
  # clean up
  unlink("tmp", recursive = TRUE)
})