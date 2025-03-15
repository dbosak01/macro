


#' @title Preprocess and Source
#' @description
#' The \code{msource} function runs the macro preprocessor
#' and then executes the program normally.
#' @param pth The path to the R program to source.
#' @import common
#' @returns The path of the sourced program.
#' @export
msource <- function(pth) {

  if (!file.exists(pth)) {
    stop(paste0("File '", pth, "' not found."))
  }

  ppth <- preprocess(pth)

  ret <- source(ppth)

  return(ret)
}


preprocess <- function(pth) {

  # Create temp file name
  bs <- basename(pth)
  npth <- file.path(tempdir(), bs)

  print(npth)

  # Kill if exists
  if (file.exists(ret))
    file.remove(ret)

  # Input program
  fl <- file(pth, open = "r", encoding = "unknown")
  lns <- readLines(fl)
  close(fl)

  # Process lines
  nlns <- mprocess(lns)

  # Output program
  fl <- file(npth, open = "w", encoding = "native.enc")
  lns <- writeLines(fl, con = fl, useBytes = TRUE)
  close(fl)

  ret <- npth

  return(ret)
}

mprocess <- function(lns) {

  ret <- lns

  return(ret)

}


