# collateral: Reproducible, comfortable and fast processing with R
[![Package-License](https://img.shields.io/github/license/mashape/apistatus.svg)](https://opensource.org/licenses/MIT)

Current version: 0.1.2 (28 November 2017)

![](vignettes/pi_progress.gif)


## Introduction

`collateral` provides you with functions you might miss in R, such as

- `p*apply()` functions for reproducible parallel processing with rich sets of features.
- `%onerror%`, `%onwarning%` operators for handling errors in a more common way.
- `throw()` let you _throw_ errors which can contain arbitrary objects - similar to Java.

Please see the **Features / Examples** section for more information.

However, this package is a proof of concept and please use it, at the moment, **for testing purposes only**. More detailed information will be provided soon.


## Installation

Up to now, `collateral` is not available on CRAN because it needs some more tests for errors to provide a high level of stability. However, you might install (or update) the latest version:

- Install the most recent version:

    ```r
    devtools::install_github("ratmaster/collateral")
    ```

- When the installation is finished, you can load the package:

    ```r
    library(collateral)
    ```

- If you find any bugs or if you have some ideas of new features, please
  consider creating an Github issue or contacting me directly by mail.


## Features / Examples

- Run loops parallely in a simple way
- Reproducible computations independent by the number of processes / workers
- Progress bars, different styles
- Estimated or elapsed time for results
- Estimated time of availability (ETA) for results
- Record and resume computations
- Memorize outputs by inputs

### Compute serially or parallely and show progress

Compute _Pi_ by a [Monte Carlo experiment](https://en.wikipedia.org/wiki/Monte_Carlo_method) serially and parallely.

You can observe:

- `psapply()` returns the same result as `sapply()`.
- The first call computes with **one** process.
- It shows a **progress bar** with **estimated time** and **ETA**.
- The second call computes with **three** processes.
- It shows a **simplified bar** with **new title** and **elapsed time** - suitable in batch mode.

```r
library(collateral)

comp_pi <- function(n) {
  x <- runif(n)
  y <- runif(n)
  inside <- sum(sqrt(x^2 + y^2) < 1)
  PI <- (inside / n) * 4
  return(PI)
}

set.seed(1234)
v1 <- psapply(rep(10000000, 20), comp_pi,
              progress = TRUE,
              time = TRUE,
              eta = TRUE)

set.seed(1234)
v2 <- psapply(rep(10000000, 20), comp_pi,
              threads = 3,
              progress = "batch",
              title = "Multiple threads",
              time = TRUE)

identical(v1, v2)
abs(pi - mean(v1))
```

![](vignettes/pi_progress.gif)


### Record and resume progress

With `p*apply()`, you can pause (stop) any loop and resume it later.

You can observe:

- The option `resume = TRUE` lets `psapply()` record the results per iteration reproducible in a folder "tmp".
- If your procedure stops (e.g. user interaction), you can **resume** the progress by rerunning the command again.
- Of course, you can change and combine further parameters, e.g. you could set `threads = 2`.

```r
library(collateral)

somefun <- function(n) {
  if (n == 40 && errorflag) {
    stop("ERROR")
  }
  Sys.sleep(0.1)
  return(n)
}

# first run
set.seed(1234)
errorflag <- TRUE
v1 <- psapply(1:100, somefun,
              resume = TRUE,
              progress = TRUE,
              time = TRUE,
              eta = TRUE)

# second run
set.seed(1234)
errorflag <- FALSE
v <- psapply(1:100, somefun,
             resume = TRUE,
             progress = TRUE,
             time = TRUE,
             eta = TRUE)

identical(v, 1:100)
```

![](vignettes/resume.gif)


### Memorize outputs

Speed up your computations by means of memorizing the output of the function by its inputs per iteration.

**CAUTION: ONLY SET THIS OPTION IF YOUR FUNCTION DOES NOT DEPAND ON RANDOM NUMBERS!**

You can observe:

- The second call of `psapply()` with `memo = TRUE` proceeds much faster
- After computing _output1_ for _input1_, `psapply()` remembers _output1_ (in the cache)
- When you the function to _input1_ again, `psapply()` recalls _output1_ without re-computing it

```r
library(collateral)

sleepy_sqrt <- function(n) {
  Sys.sleep(0.1)
  return(sqrt(n))
}

set.seed(1234)
x <- ceiling(runif(100) * 10)
head(x)

# without cache
v1 <- psapply(x, sleepy_sqrt,
              progress = TRUE,
              time = TRUE)
identical(v1, sqrt(x))

# with cache
v2 <- psapply(x, sleepy_sqrt,
              memo = TRUE,
              progress = TRUE,
              time = TRUE)
identical(v2, sqrt(x))
```

![](vignettes/memo.gif)


## Documentation

Work in progress ... Please have a look at:

```r
library(collateral)
?collateral
?plapply
```

## Author

Fabian Raters.


## License

MIT, 2016-2017.
