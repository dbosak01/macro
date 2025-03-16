
e <- new.env()

#' @title Preprocess and Source
#' @description
#' The \code{msource} function runs the macro preprocessor
#' and then executes the program normally.
#' @param pth The path to the R program to source.
#' @import common
#' @returns The path of the sourced program.
#' @export
msource <- function(pth) {

  #browser()

  if (!file.exists(pth)) {
    stop(paste0("File '", pth, "' not found."))
  }

  ppth <- preprocess(pth)

  ret <- source(ppth)

  return(ret)
}

#' @noRd
preprocess <- function(pth) {

  # browser()

  # Create temp file name
  bs <- basename(pth)
  npth <- file.path(tempdir(), bs)

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

  isopen <- NULL

  for (ln in lns) {

    islet <- is_let(ln)

    if (!islet) {



      isif <-  is_if(ln)
      iselseif <- is_elseif(ln)

      if (as.logical(isif)) {
        if (attr(isif, "value") == TRUE) {
          isopen <- TRUE
        } else {

          isopen <- FALSE
        }
       } else if (as.logical(iselseif)) {
        if (attr(iselseif, "value") == TRUE) {
          isopen <- TRUE
        } else {

          isopen <- FALSE
        }
      } else if (is_else(ln)) {

        isopen <- TRUE
      } else if (is_end(ln)) {
        isopen <- NULL
      } else if (is.null(isopen)) {

        ret <- append(ret, mreplace(ln))
      } else if (isopen == TRUE) {

        ret <- append(ret, mreplace(ln))
      }
    }
  }


  return(ret)

}


mreplace <- function(ln) {

  # browser()
  ret <- ln
  vrs <- ls(envir = e)
  if (length(vrs) > 0) {
    for (vr in vrs) {

      ret <- gsub(vr, e[[vr]], ret, fixed = TRUE)
    }
  }

  return(ret)
}


