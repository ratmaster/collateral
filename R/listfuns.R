#' @export
#' @title Creates empty named list
#' @description Creates empty (named) list.
emptylist <- function(n = NULL, along = NULL, named = NULL) {
  if (!is.null(n)) {
    return(vector("list", n))
  } else if (!is.null(along)) {
    x <- vector("list", length(along))
    names(x) <- names(along)
    return(x)
  } else if (!is.null(named)) {
    x <- vector("list", length(named))
    names(x) <- named
    return(x)
  } else {
    return(list())
  }
}
