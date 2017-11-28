#' @export
#' @title Parallel, reproducible lapply function with comfort
#' @description Parallel, reproducible version of R's \code{lapply()} function.
#'              \strong{Proof of concept - for testing purposes only}
#'
#' @param X            A vector or list. Any object that can be coerced by \code{as.list}.
#'
#' @param FUN          The function to be applied to each element of X.
#'
#' @param ...          Additional arguments to \code{FUN}.
#'
#' @param progress     A logical value or a cahracter string. If TRUE or "bar", a modern looking
#'                     progress bar shows the status of the computations. Further options are
#'                     "batch" and "simple" which print simpler progress bars.
#'
#' @param title        The printed title of the progress bar.
#'
#' @param memo         A logical value. If TRUE, \code{FUN} is transformed to memorize its
#'                     output per distinct input arguments in an internal cache. If
#'                     \code{FUN} is applied to the same input again, it returns the corresponding
#'                     cached pre-computed output (see Details).
#'
#' @param resume       A logical value. If TRUE, the function records the output per iteration
#'                     in the folder "tmp". If you re-run the function, it will resume the computations.
#'
#' @param eta          A logical value. If TRUE, the estimated time (and date) of availability (ETA) is printed
#'
#' @param time         A logical value. If TRUE, the estimated and the elapsed time of the progress are printed.
#'
#' @param threads      An integer. The number of threads, i.e. parallel processes, to employ.
#'                     Caution: At the moment, only Unix based systems are supported for threads > 1
#'                     (see Details).
#'
#' @param sameSeed     A logical value. If TRUE, the same seed is set before each iteration.
#'
#' @param stopOnError  A logical value. If TRUE, the execution stops immediately in case of an error. Otherwise,
#'                     an error object in place for the failed iteration is returned.
#'
#' @param seed         An integer. Sets a random seed in the beginning.
#'
#' @param simplify     A logical value. If TRUE, the resulting list will be transformed (simplified)
#'                     by \code{simplify2array()}.
#'
#'
#' @details  Uniform clear and clean way making computations reproducible. It does not matter, whether
#'           the computations are performed serially or parallely.
#'
#'           \strong{Caution:}
#'           \enumerate{
#'             \item Currently, non-unix systems are restricted to threads = 1 because they do not support
#'                   R's parallel mechanism (forking).
#'             \item Use Option memo = TRUE ONLY for functions that do NOT DRAW RANDOM NUMBERS. Otherwise,
#'                   the result will be like the functions always proceeds with the same random draw.
#'           }
#'
#' @return   The function returns \code{TRUE} if no error occurred.
#'
#' @seealso  \code{\link{lapply}}, \code{\link{sapply}}, \code{\link{mclapply}}
#'           and \code{\link{simplify2array()}}.
#'
plapply <- function(X, FUN, ...,
                    progress = FALSE,
                    title = "Progress",
                    memo = FALSE,
                    resume = FALSE,
                    eta = FALSE,
                    time = FALSE,
                    threads = 1,
                    sameSeed = FALSE,
                    stopOnError = TRUE,
                    seed = NULL,
                    simplify = FALSE) {

  # obtaining global values for missing arguments
  opts <- getOption("collateralopts")
  if (missing(progress)) {
    progress <- opts$progress
  }
  if (missing(title)) {
    title <- opts$title
  }
  if (missing(memo)) {
    memo <- opts$memo
  }
  if (missing(resume)) {
    resume <- opts$resume
  }
  if (missing(eta)) {
    eta <- opts$eta
  }
  if (missing(time)) {
    time <- opts$time
  }
  if (missing(threads)) {
    threads <- opts$threads
  }
  if (missing(sameSeed)) {
    sameSeed <- opts$sameSeed
  }
  if (missing(stopOnError)) {
    stopOnError <- opts$stopOnError
  }

  # determine random seed
  if (!is.null(seed)) {
    if (!is.numeric(seed)) {
      stop("Parameter 'seed' (used for set.seed()) must be an integer")
    }
    set.seed(seed)
  }
  preRandom.seed <- .GlobalEnv$.Random.seed
  if (is.null(preRandom.seed)) {
    stop("No random seed is set. Please provide parameter 'seed' or",
         "use 'set.seed()'")
  }
  seed <- preRandom.seed[length(preRandom.seed)]

  # check length
  n <- length(X)
  if (n == 0) {
    # robust seed
    set.seed(seed - 1)

    return(list())
  }
  if (!is.list(X)) {
    X <- as.list(X)
  }
  FUN <- FUNO <- match.fun(FUN)

  # option: record and resume
  if (isTRUE(resume)) {
    rec <- Recorder$new(FUNO, X, seed)
  } else {
    rec <- EmptyRecorder$new(FUNO, X, seed)
  }
  stored <- rec$resume()
  Y <- stored$Y
  open <- which(!stored$done)
  done <- n - length(open)

  # option: initialize progress
  if (is.character(progress)) {
    pb <- Progress$new(value = done / n,
                       title = title,
                       type = progress,
                       eta = eta,
                       time = time)
  } else {
    if (is.logical(progress) && progress) {
      pb <- Progress$new(value = done / n,
                         title = title,
                         type = "bar",
                         eta = eta,
                         time = time)
    } else {
      pb <- EmptyProgress$new()
    }
  }

  # check for work
  if (n == done) {
    # robust seed
    set.seed(seed - 1)

    # option: simplify
    if (isTRUE(simplify)) {
      return(simplify2array(Y))
    }
    return(Y)
  }

  # option: same seed
  if (isTRUE(sameSeed)) {
    newseed <- 0
  } else {
    newseed <- 1
  }

  # forced restrictions on parameters
  # parallelization for unix only
  if (.Platform$OS.type == "unix") {
    # restrict child process to one worker
    if (parallel:::isChild()) {
      progress <- FALSE
      threads <- 1
    }
  } else {
    threads <- 1
  }

  # option: memorize function
  if (isTRUE(memo)) {
    if (!is.memoised(FUN)) {
      FUN <- memoise(FUN)
    }
  }

  # check for parallelization
  if (threads > 1 && length(open) > 1) {
    free <- threads
    jobs <- list()
    cleanup <- function() {
      if (length(jobs) > 0) {
        job_pids <- lapply(jobs, "[[", "pid")
        mccollect(parallel:::children(job_pids), FALSE)
        parallel:::mckill(parallel:::children(job_pids), SIGTERM)
        mccollect(parallel:::children(job_pids))
      }
    }
    on.exit(cleanup())
    repeat {
      while (free > 0 && length(open) > 0) {
        i <- open[1]
        jobs <- c(jobs, list(runParallel(i, FUN, X[[i]], seed, newseed, ...)))
        free <- free - 1
        open <- open[-1]
      }
      res <- pollResults(jobs, stopOnError = stopOnError)
      if (length(res$id) > 0) {
        Y[res$id] <- res$val
        m <- length(res$id)
        for (j in 1:m) {
          rec$record(res$id[j], res$val[[j]])
        }
        done <- done + m
        pb$set(done / n)
        if (done == n) {
          break
        }
        jobs[res$jkill] <- NULL
        free <- free + m
      }
      # relieve processor
      Sys.sleep(0.1)
    }
  } else {
    # option: stop on error
    if (isTRUE(stopOnError)) {
      FUNI <- FUN
    } else {
      FUNI <- function(x, ...) {return(try(FUN(x, ...), silent = TRUE))}
    }
    for (i in open) {
      set.seed(seed + newseed * i)
      y <- FUNI(X[[i]], ...)
      if (!is.null(y)) {
        Y[[i]] <- y
      }
      rec$record(i, y)
      done <- done + 1
      pb$set(done / n)
    }
  }

  # robust seed
  set.seed(seed - 1)

  # option: simplify
  if (isTRUE(simplify)) {
    return(simplify2array(Y))
  }
  return(Y)
}


# run function with a list of inputs parallely
runParallel <- function(id, FUN, x, seed, newseed, ...) {
  parFun <- function() {
    # set seed
    set.seed(seed + newseed * id)

    # iteratively apply function to list
    return(FUN(x, ...))
  }
  return(list(id = id,
              pid = mcparallel(expression(parFun()), mc.set.seed = FALSE,
                               silent = TRUE, mc.affinity = NULL,
                               mc.interactive = FALSE, detached = FALSE)))
}


# poll processes for collecting results
pollResults <- function(jobs, stopOnError) {
  ids <- numeric()
  jkills <- numeric()
  vals <- list()
  for (i in seq_along(jobs)) {
    res <- mccollect(jobs[[i]]$pid, wait = FALSE)

    # check result availability
    if (!is.null(res)) {
      # signal the job as terminating
      mccollect(jobs[[i]]$pid, wait = TRUE)

      # get single result
      res <- res[[1]]

      # check for error
      if (isTRUE(stopOnError) && inherits(res, "try-error")) {
        stop(sprintf("Encountered error while computing iteration %i: %s",
                     jobs[[i]]$id, trimws(as.character(res))))
      }

      # save to results list
      ids <- c(ids, jobs[[i]]$id)
      jkills <- c(jkills, i)
      vals <- c(vals, list(res))
    }
  }
  return(list(id = ids, jkill = jkills, val = vals))
}
