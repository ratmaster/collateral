#' collateral: Reproducible, comfortable and fast processing with R
#'
#' \code{collateral} provides you with functions you might miss in R.
#'
#' \strong{Overview:}
#' \itemize{
#'   \item \code{p*apply()} functions for reproducible parallel processing.
#'   \item \code{\%onerror\%}, \code{\%onwarning\%} operators for handling errors in a more common way.
#'   \item \code{throw} let you \emph{throw} errors which can contain arbitrary objects - similar to Java.
#' }
#'
#' \strong{However, this package is a proof of concept and please use it,
#' at the moment, for testing purposes only}
#'
#' @seealso \code{\link{plapply}}, \code{\link{\%onerror\%}} and \code{\link{throw}}
#'
#' @docType package
#' @importFrom digest digest
#' @importFrom memoise is.memoised memoise
#' @import parallel
#' @importFrom tools SIGTERM
#' @importFrom R6 R6Class
#' @name collateral
NULL
