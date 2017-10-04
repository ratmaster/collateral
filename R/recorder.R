Recorder  <-
  R6::R6Class(
    "Recorder",
    public = list(
      # methods

      initialize = function(FUN, X, seed) {
        # create empty results list
        private$Y <- emptylist(along = X)

        # hash objects
        sd <- digest(seed, algo = "md5")
        xd <- digest(X, algo = "md5")
        fd <- digest(list(formals(FUN), body(FUN)), algo = "md5")
        path <- paste0("tmp/", sd, "_", xd, "_", fd)

        # ensure that folder exists
        if (!dir.exists(path)) {
          if (!dir.create(path, recursive = TRUE)) {
            stop("Cannot create ", path, " folder!")
          }
        }
        private$path <- path
      },


      finalize = function() {
      },


      record = function(id, y) {
        saveRDS(list(id = id, y = y),
                file = paste0(private$path, "/", as.character(id)))
        return(invisible(self))
      },


      resume = function() {
        Y <- private$Y
        done <- logical(length(Y))
        files <- list.files(private$path, pattern = "^\\d+$", full.names = TRUE)
        for (file in files) {
          obj <- readRDS(file)
          Y[[obj$id]] <- obj$y
          done[obj$id] <- TRUE
        }
        return(list(Y = Y, done = done))
      }
    ),


    private = list(
      # attributes

      path = NULL,
      Y = NULL
      # methoden
    )
  )


EmptyRecorder  <-
  R6::R6Class(
    "EmptyRecorder",
    public = list(
      # methods

      initialize = function(FUN, X, seed) {
        # create empty results list
        private$Y <- emptylist(along = X)
      },


      finalize = function() {
      },


      record = function(id, y) {
        return(invisible(self))
      },


      resume = function() {
        return(list(Y = private$Y,
                    done = logical(length(private$Y))))
      }
    ),


    private = list(
      # attributes

      Y = NULL

      # methoden
    )
  )
