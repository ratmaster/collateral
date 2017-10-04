#' @export
#' @title Try blocks for errors
#' @description Try expression blocks
#'              Use 'stop(e)' to throw error and stop.
#'              Use 'message(e)' to just throw the error message.
`%onerror%` <- function(expr, err) {
  envir <- parent.frame()
  tryCatch(expr,
           error = function(e) {
             assign("e", e, envir = envir)
             err
           })
}


#' @export
#' @title Try blocks for warnings
#' @description Try expression blocks
#'              Use 'warning(w)' to throw warning.
`%onwarning%` <- function(expr, warn) {
  envir <- parent.frame()
  tryCatch(expr,
           warning = function(w) {
             assign("w", w, envir = envir)
             warn
           })
}


#' @export
#' @title Throw error with objects
#' @description Throw error with objects
throw <- function(..., call = NULL, collapse = " ") {
  args <- list(...)
  argsn <- names(args)
  if (is.null(argsn)) {
    if (length(args) == 1L && inherits(args[[1L]], "condition")) {
      message <- conditionMessage(args[[1L]])
      call <- conditionCall(args[[1L]])
    } else {
      message <- paste0(..., collapse = collapse)
    }
    cond <- structure(list(message = message,
                           call = call),
                      class = c("throwError", "error", "condition"))
  } else {
    v <- argsn == ""
    message <- paste(args[v], collapse = collapse)
    cond <- structure(c(list(message = message,
                             call = call), args[!v]),
                      class = c("throwError", "error", "condition"))
  }
  .Internal(.signalCondition(cond, message, call))
  .Internal(.dfltStop(message, call))
}