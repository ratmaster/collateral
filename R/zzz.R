.onLoad <- function(libname, pkgname){
  opts <- list(progress = FALSE,
               title = "Progress",
               memo = FALSE,
               resume = FALSE,
               eta = FALSE,
               time = FALSE,
               threads = 1,
               sameSeed = FALSE,
               stopOnError = TRUE)
  pre <- getOption("collateralopts")
  if (!is.null(pre) && is.list(pre)) {
    opts[names(pre)] <- pre
  }
  options("collateralopts" = opts)
  invisible(NULL)
}


.onUnload <- function(libpath){
  options("collateralopts" = NULL)
  invisible(NULL)
}
