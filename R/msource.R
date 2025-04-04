
e <- new.env()

#' @title Preprocess and Source
#' @description
#' The \code{msource} function runs the macro preprocessor
#' and then executes the program normally.
#' @param pth The path to the R program to source.
#' @param file_out If you want to save or view the generated code
#' from the \code{msource} function, supply a full path and
#' file name on this parameter.
#' @import common
#' @returns The path of the sourced program.
#' @export
msource <- function(pth, file_out = NULL) {

  #browser()

  if (!file.exists(pth)) {
    stop(paste0("File '", pth, "' not found."))
  }

  ppth <- preprocess(pth, file_out)

  ret <- source(ppth)

  return(ret)
}

#' @noRd
preprocess <- function(pth, file_out) {

  # browser()

  # Create output file name
  if (is.null(file_out)) {
    bs <- basename(pth)
    npth <- file.path(tempdir(), bs)
  } else {
    npth <- file_out
  }

  print(npth)

  # Kill if exists
  if (file.exists(npth))
    file.remove(npth)

  # Input program
  fl1 <- file(pth, open = "r", encoding = "UTF-8")
  lns <- readLines(fl1)
  close(fl1)

  # Process lines
  nlns <- mprocess(lns)

  # Output program
  fl2 <- file(npth, open = "w", encoding = "native.enc")
  writeLines(nlns, con = fl2, useBytes = TRUE)
  close(fl2)

  ret <- npth

  return(ret)
}



#' @noRd
mprocess <- function(lns) {

  e <- new.env()
  ret <- c()

  lvl <- 1 # lvl 1 is open code
  isopen <- list()
  isopen[[lvl]] <- TRUE
  elseflag <- list()
  elseflag[[lvl]] <- TRUE
  lncnt <- 1

  for (ln in lns) {

    ln <- sub_sysfunc(ln)

    islet <- is_let(ln)

    if (!islet) {
      # browser()
      isif <-  is_if(ln)
      iselseif <- is_elseif(ln)

      if (as.logical(isif)) {
        lvl <- lvl + 1
        if (isopen[[lvl - 1]]) {
          if (attr(isif, "value") == TRUE) {
            isopen[[lvl]] <- TRUE
            elseflag[[lvl]] <- FALSE
          } else {
            isopen[[lvl]]  <- FALSE
          }
        }
      } else if (as.logical(iselseif)) {
        if (isopen[[lvl - 1]]) {
          if (attr(iselseif, "value") == TRUE) {
            isopen[[lvl]]  <- TRUE
            elseflag[[lvl]] <- FALSE
          } else {
            isopen[[lvl]]  <- FALSE
          }
        }
      } else if (is_else(ln)) {
        if (isopen[[lvl - 1]]) {
          if (length(elseflag) < lvl)
            elseflag[[lvl]] <- TRUE

          if (elseflag[[lvl]]) {
            isopen[[lvl]]  <- TRUE
          } else {
            isopen[[lvl]]  <- FALSE
          }
        }
      } else if (is_end(ln)) {
        isopen[[lvl]]  <- FALSE
        elseflag[[lvl]] <- TRUE
        lvl <- lvl - 1
      } else if (isopen[[lvl]] == TRUE) {
        ret <- append(ret, mreplace(ln))
      }
    }

    print(paste0("Line: ", lncnt, ", Level: ", lvl, ", isopen: ", isopen[[lvl]]))
    lncnt <- lncnt + 1
  }


  return(ret)

}


mreplace <- function(ln) {

  ret <- ln

  if (nchar(ln) > 0) {
    # browser()

    vrs <- ls(envir = e)
    if (length(vrs) > 0) {
      for (vr in vrs) {

        # vl <- deparse1(e[[vr]])
        vl <- e[[vr]]
        ret <- gsub(vr, vl, ret, fixed = TRUE)
      }
    }
  }

  return(ret)
}

# re1 <- "x."
# sm1 <- c('one', 'two', 'three')
# sm2 <- deparse1(sm1)
# ret <- gsub("x.", sm2, re1, fixed = TRUE)
