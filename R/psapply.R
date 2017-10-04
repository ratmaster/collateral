#' @export
#' @title Parallel sapply wrapper function
#' @description Parallel sapply wrapper function for plapply(..., simplify = TRUE).
#'              \strong{Proof of concept - for testing purposes only}
psapply <- function(X, FUN, ...) {
  return(plapply(X, FUN, ..., simplify = TRUE))
}