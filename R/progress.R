Progress <-
  R6::R6Class(
    "Progress",
    public = list(
      # methods

      initialize = function(value = 0,
                            title = "Progress",
                            type = "bar",
                            eta = FALSE,
                            time = FALSE,
                            width = NULL) {
        # setermine title display
        private$title <- strtrim(title, 20)
        private$ntitle <- nchar(title) + 2

        # check type of execution
        if (interactive()) {
          private$type <- type
        } else {
          private$type <- "batch"
        }

        # size time display
        if (eta) {
          if (time) {
            private$ntime <- 21
          } else {
            private$ntime <- 13
          }
        } else {
          if (time) {
            private$ntime <- 9
          }
        }

        # size display
        if (is.null(width)) {
          width <- getOption("width")
        }
        private$width <- width

        # size progress display
        if (type == "bar") {
          private$nbar <- width - private$ntitle - private$ntime - 11
        } else if (type == "batch") {
          private$nbar <- width - private$ntitle - private$ntime - 9
        }
        if (private$nbar < 1) {
          private$type <- "simple"
          private$nbar <- width
        }

        # set and print status
        self$set(value)
      },


      finalize = function() {
      },


      print = function() {
        if (private$type == "bar") {
          private$print_bar()
        } else if (private$type == "batch") {
          private$print_batch()
        } else {
          private$print_simple()
        }
        if (private$values[private$n] == 100) {
          cat("\n")
        }
        flush.console()
      },


      set = function(value) {
        private$values <- c(private$values, value * 100)
        if (private$ntime > 0) {
          private$times <- c(private$times, as.numeric(proc.time())[3])
        }
        private$n <- private$n + 1
        self$print()
      }
    ),


    private = list(
      # attributes

      values = NULL,
      times = NULL,
      n = 0,
      type = NULL,
      title = NULL,
      width = NULL,
      ntitle = 0,
      ntime = 0,
      nbar = 0,


      # methoden

      estimate_secs = function() {
        X <- cbind(rep.int(1, private$n), private$values)
        b <- crossprod(solve(crossprod(X)), crossprod(X, private$times))
        return(max(0, sum(c(1, 100) * b) - private$times[private$n]))
      },


      print_bar = function() {
        n <- private$n
        lastval <- private$values[n]

        # compute times
        if (private$ntime > 0 ) {
          if (private$ntime == 9) {
            # time elapsed
            timestr <- sprintf("   %6s",
                               str_secs(private$times[n] - private$times[1]))
          } else {
            # time eta
            if (n < 3) {
              if (lastval == 100) {
                if (private$ntime == 13) {
                  timestr <- sprintf("  %10s\U25A3", str_eta(0))
                } else {
                  timestr <- sprintf("   %6s %10s\U25A3",
                                     str_secs(private$times[n] - private$times[1]),
                                     str_eta(0))
                }
              } else {
                # not enough data
                timestr <- ""
              }
            } else {
              if (private$ntime == 13) {
                if (lastval == 100) {
                  timestr <- sprintf("  %10s\U25A3", str_eta(0))
                } else {
                  timestr <- sprintf("  %10s\U25C9", str_eta(private$estimate_secs()))
                }
              } else {
                if (lastval == 100) {
                  timestr <- sprintf("   %6s %10s\U25A3",
                                     str_secs(private$times[n] - private$times[1]),
                                     str_eta(0))
                } else {
                  eta <- private$estimate_secs()
                  timestr <- sprintf("  ~%6s %10s\U25C9", str_secs(eta), str_eta(eta))
                }
              }
            }
          }
        } else {
          timestr <- ""
        }

        # print progress bar
        i <- round(lastval / 100 * private$nbar)
        cat("\r", private$title, "  \U2503",
            rep("\U2593", i),
            rep("\U2591", private$nbar - i),
            sprintf("\U2503 %6.2f %%", lastval),
            timestr, sep = "")
      },


      print_batch = function() {
        n <- private$n
        if (n == 1) {
          cat(private$title, "  ", sep = "")
        }
        private$print_simple()
        if (private$values[n] == 100) {
          cat(sprintf(" %6.2f %%", 100))
          if (private$ntime > 0) {
            if (private$ntime == 9) {
              cat(sprintf("   %6s",
                          str_secs(private$times[n] - private$times[1])))
            } else if (private$ntime == 13) {
              cat(sprintf("  %10s#", str_eta(0)))
            } else {
              cat(sprintf("   %6s %10s#",
                          str_secs(private$times[n] - private$times[1]),
                          str_eta(0)))
            }
          }
        }
      },


      print_simple = function() {
        n <- private$n
        if (n > 1) {
          i <- diff(round(private$values[(n - 1):n] / 100 * private$nbar))
        } else {
          i <- round(private$values[1] / 100 * private$nbar)
        }
        if (i > 0) {
          cat(rep(".", i), sep = "")
        }
      }
    )
  )


EmptyProgress  <-
  R6::R6Class(
    "EmptyProgress",
    public = list(
      # methods

      initialize = function() {
      },


      finalize = function() {
      },


      print = function() {
      },


      set = function(value) {
      }
    ),


    private = list(
      # attributes
      # methoden
    )
  )


str_eta <- function(secs) {
  if (secs < 86400) {
    a <- "%H:%M:%S"
  } else if (secs < 604800) {
    a <- "%a, %H:%M"
  } else if (secs < 2592000) {
    a <- "%a, %d %b"
  } else {
    a <- "%x"
  }
  return(format(Sys.time() + secs, a))
}


str_secs <- function(secs) {
  mins <- secs %/% 60
  if (mins == 0) {
    return(sprintf("%5.2fs", secs))
  }
  secs <- round(secs %% 60)
  hours <- mins %/% 60
  if (hours == 0) {
    return(sprintf("%2i:%02im", mins, secs))
  }
  mins <- mins %% 60
  days <- hours %/% 24
  if (days == 0) {
    return(sprintf("%2i:%02ih", hours, mins))
  }
  hours <- hours %% 24
  if (days < 100) {
    return(sprintf("%2i.%02id", days, round(hours / 24 * 100)))
  }
  years <- days %/% 365.25
  if (years == 0) {
    return(sprintf("%5id", days))
  }
  if (years < 100) {
    days <- days %% 24
    return(sprintf("%2i.%02iy", years, round(days / 365.25 * 100)))
  }
  if (years <= 1000) {
    return(sprintf("%5iy", years))
  }
  return(">1000y")
}
