#' @export
#' @title Set up parallel environment from here
#' @description Set up parallel environment from here.
#'
#' @details Set global options.
#'
#' @seealso \code{\link{plapply}} and \code{\link{psapply}}.
from_here <- function(progress = FALSE,
                      title = "Progress",
                      memo = FALSE,
                      resume = FALSE,
                      eta = FALSE,
                      time = FALSE,
                      threads = 1,
                      sameSeed = FALSE,
                      stopOnError = TRUE) {
  opts <- getOption("collateralopts")
  pre <- opts
  x <- as.list(match.call())[-1]
  if (length(x) > 0) {
    opts[names(x)] <- x
    options("collateralopts" = opts)
  }
  return(invisible(pre))
}