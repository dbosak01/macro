
e <- new.env()

#' @title Preprocess and Source
#' @description
#' The \code{msource} function runs the macro preprocessor
#' and then executes the program normally.
#' @param pth The path to the R program to source.
#' @param file_out If you want to save or view the generated code
#' from the \code{msource} function, supply a full path and
#' file name on this parameter. Default is NULL. When NULL,
#' the function will create a temp file for the generated code.
#' @param envir The environment to be used for program execution.
#' Default is the global environment.
#' @param ... Follow-on parameters to the \code{source} function. See
#' the \code{\link{source}} function for additional information.
#' @import common
#' @returns The path of the sourced program.
#' @export
msource <- function(pth = Sys.path(), file_out = NULL, envir = globalenv(),
                    ...) {

  #browser()

  if (!file.exists(pth)) {
    stop(paste0("File '", pth, "' not found."))
  }

  if (is.null(envir))
    stop("Environment cannot be null.")
  else
    e <- envir


  ppth <- preprocess(pth, file_out, envir)

  ret <- source(ppth, local = e, ...)

  invisible(ret)
}

# Input macro code and output path to resolved code
#' @noRd
preprocess <- function(pth, file_out, envir) {

  # browser()

  # Create output file name
  if (is.null(file_out)) {
    bs <- basename(pth)
    npth <- file.path(tempdir(), bs)
  } else {
    npth <- file_out
  }

  # print(npth)

  # Kill if exists
  if (file.exists(npth))
    file.remove(npth)

  # Input program
  fl1 <- file(pth, open = "r", encoding = "UTF-8")
  lns <- readLines(fl1)
  close(fl1)

  # Clear out environment
  enms <- ls(e)
  if (length(enms) > 0) {
    rm(list = enms, envir = e)
  }

  # Copy any macro variables to new environment
  vrs <- ls(envir)
  mvrs <- grepl("\\.$", vrs)
  vrs <- vrs[mvrs]

  for (nm in vrs) {
    #ne[[nm]] <- envir[[nm]]
    assign(nm, envir[[nm]], envir = e)
  }

  # print(ls(e))

  # Process lines
  nlns <- mprocess(lns)

  # Output program
  fl2 <- file(npth, open = "w", encoding = "native.enc")
  writeLines(nlns, con = fl2, useBytes = TRUE)
  close(fl2)

  ret <- npth

  return(ret)
}


# Process macro statements
#' @noRd
mprocess <- function(lns) {

  ret <- c()

  # print(ls(e))

  lvl <- 1 # lvl 1 is open code
  isopen <- list()
  isopen[[lvl]] <- TRUE
  elseflag <- list()
  elseflag[[lvl]] <- TRUE
  idx <- 1
  lncnt <- length(lns)

  while (idx <= lncnt) {

    # Initialize line
    ln <- lns[idx]

    # Resolve sysfuncs
    ln <- sub_sysfunc(ln)

    # Identify let statements
    islet <- is_let(ln)

    if (!islet) {

      # browser()

      # If flag
      isif <-  is_if(ln)

      # Ifelse flag
      iselseif <- is_elseif(ln)

      # Include flag
      isinclude <- is_include(ln)

      # Next line macro
      if (idx < lncnt) {
        nl <- lns[idx + 1]
        if (is_comment(nl) | trimws(nl) == "") {
          ismacro <- TRUE
        } else {
          ismacro <- FALSE
        }
      } else {
        ismacro <- FALSE
      }

      if (as.logical(isif)) {   # Deal with 'if'
        lvl <- lvl + 1
        if (isopen[[lvl - 1]]) {
          if (attr(isif, "value") == TRUE) {
            isopen[[lvl]] <- TRUE
            elseflag[[lvl]] <- FALSE
          } else {
            isopen[[lvl]]  <- FALSE
          }
        }
      } else if (as.logical(iselseif)) {   # Deal with 'ifelse'
        if (isopen[[lvl - 1]]) {
          if (attr(iselseif, "value") == TRUE) {
            isopen[[lvl]]  <- TRUE
            elseflag[[lvl]] <- FALSE
          } else {
            isopen[[lvl]]  <- FALSE
          }
        }
      } else if (is_else(ln)) {   # Deal with 'else'
        if (isopen[[lvl - 1]]) {
          if (length(elseflag) < lvl)
            elseflag[[lvl]] <- TRUE

          if (elseflag[[lvl]]) {
            isopen[[lvl]]  <- TRUE
          } else {
            isopen[[lvl]]  <- FALSE
          }
        }
      } else if (is_end(ln)) {   # Deal with 'end'
        isopen[[lvl]]  <- FALSE
        elseflag[[lvl]] <- TRUE
        lvl <- lvl - 1
      } else if (as.logical(isinclude)) {  # Deal with 'include'

        if (isopen[[lvl]] == TRUE) {

          # Resolve any macro variables
          pth <- mreplace(attr(isinclude, "path"))

          # Get lines from include file
          nlns <- get_include(pth)

          # Insert into lns vector
          lns <- c(lns[seq(1, idx - 1)], nlns, lns[seq(idx + 1, lncnt)])

          # Reset line count
          lncnt <- length(lns)

          # Reset index
          idx <- idx - 1

        }
      } else if (is_comment(ln)) {  # Deal with macro comment
        # Do nothing
        # Don't emit
      } else if (isopen[[lvl]] == TRUE) {   # Deal with normal code

        if (nchar(trimws(ln)) == 0 & ismacro) {
          # Do nothing
          # Eliminate blank lines before macro statements
        } else {
          # If it makes it to this point,
          # replace any macro variables and emit as code
          ret <- append(ret, mreplace(ln))
        }
      }
    }

    # print(paste0("Line: ", idx, ", Level: ", lvl, ", isopen: ", isopen[[lvl]]))
    idx <- idx + 1
  }


  return(ret)

}


mreplace <- function(ln) {

  ret <- ln

  if (nchar(ln) > 0) {
    # browser()

    # print(ls(e))

    vrs <- ls(e)  # ls(envir = e)
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
