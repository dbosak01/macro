
# Macro symbol table
e <- new.env()

# Separator for debug output
strs <- paste0(rep("*", 80), collapse = "")

#' @title Pre-process and Source
#' @description
#' The \code{msource} function runs the macro pre-processor
#' and then executes the program normally.
#' @details
#' Additional details...
#'
#' @section Macro Commands:
#' Below is the complete list of available macro commands:
#' \itemize{
#'   \item{\strong{#%&lt;comment&gt;}: A macro comment.}
#'   \item{\strong{#%include '&lt;path&gt;'}: Inserts code from included file as text
#'   into current program.}
#'   \item{\strong{#%if (&lt;condition&gt;)}: Begins a macro conditional block.}
#'   \item{\strong{#%elseif (&lt;condition&gt;)}: Defines a subsequent conditional block.}
#'   \item{\strong{#%else}: Identifies the default behavior in a condition.}
#'   \item{\strong{#%end}: Ends a macro condition.}
#'   \item{\strong{%sysfunc(&lt;expression&gt;)}: Executes an R expression as part of
#'   a macro command.}
#' }
#'
#' @param pth The path to the R program to source.
#' @param file_out If you want to save or view the generated code
#' from the \code{msource} function, supply a full path and
#' file name on this parameter. Default is NULL. When NULL,
#' the function will create a temp file for the generated code.
#' @param envir The environment to be used for program execution.
#' Default is the global environment.
#' @param exec Whether or not to execute the output file after pre-processing.
#' Default is TRUE
#' @param debug If TRUE, prints lines to the console as they are processed.
#' This information case be useful for debugging macro code.
#' Default is FALSE.  See the \code{vignette("macro-debug"} for more information
#' on debugging.
#' @param debug_out A path to a file to be used for debugging.  If a path
#' is supplied, debug output will be written to the file instead of the
#' console. Default is NULL.
#' @param ... Follow-on parameters to the \code{source} function. See
#' the \code{\link{source}} function for additional information.
#' @import common
#' @returns The results of the \code{source()} function, invisibly.  The path
#' of the resolved output file is also included under the "$output" list item.
#' @export
msource <- function(pth = Sys.path(), file_out = NULL, envir = globalenv(),
                    exec = TRUE, debug = FALSE, debug_out = NULL,
                    ...) {

  #browser()

  if (!file.exists(pth)) {
    stop(paste0("File '", pth, "' not found."))
  }

  if (is.null(envir))
    stop("Environment cannot be null.")
  else
    e <- envir

  if (debug) {
    cat(strs, "\n")
    cat("**  Pre-Processing\n")
    cat(strs, "\n")
    cat("-    File In: ", pth, "\n")
  }

  ppth <- preprocess(pth, file_out, envir, debug)

  if (exec) {
    if (debug) {
      cat(strs, "\n")
      cat("**  Execution\n")
      cat(strs, "\n")

      ret <- source(ppth, local = e, echo = TRUE, ...)
    } else {
      ret <- source(ppth, local = e, ...)
    }
  } else {

    ret <- list()
    ret$value <- NA
  }

  # Clean up temporary files
  if (is.null(file_out)) {
    file.remove(ppth)
    ret$output <- ""
  } else {
    # Add output path to return object
    ret$output <- ppth
  }

  if (debug) {
    cat("\n")
    cat(strs, "\n")
    cat("**  End\n")
    cat(strs, "\n")
  }

  invisible(ret)
}

# Input macro code and output path to resolved code
#' @noRd
preprocess <- function(pth, file_out, envir, debug) {

  # Create output file name
  if (is.null(file_out)) {
    bs <- basename(pth)
    npth <- file.path(tempdir(), bs)
  } else {
    npth <- file_out
  }

  # print(npth)
  if (debug) {
    cat("-   File Out: ", npth, "\n")
    cat(strs, "\n")
    cat("[ In#][Out#]:\n")
  }

  # Kill if exists
  if (file.exists(npth))
    file.remove(npth)

  # Input program
  fl1 <- file(pth, open = "r", encoding = "UTF-8")
  lns <- readLines(fl1, warn = FALSE)
  close(fl1)

  # Clear out macro environment (symbol table)
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
  nlns <- mprocess(lns, debug)

  # Output program
  fl2 <- file(npth, open = "w", encoding = "native.enc")
  writeLines(nlns, con = fl2, useBytes = TRUE)
  close(fl2)

  ret <- npth

  return(ret)
}


# Process macro statements
#' @noRd
mprocess <- function(lns, debug) {

  ret <- c()

  # print(ls(e))

  lvl <- 1 # lvl 1 is open code
  isopen <- c()
  isopen[lvl] <- TRUE
  elseflag <- c()
  elseflag[lvl] <- TRUE
  idx <- 1
  idxO <- 0
  lncnt <- length(lns)
  emt <- FALSE

  while (idx <= lncnt) {

    # Initialize line
    ln <- lns[idx]

    # Resolve sysfuncs
    ln <- sub_funcs(ln)

    # Identify let statements
    islet <- is_let(ln, isopen[lvl])

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
        elseflag[lvl] <- TRUE # Default else to TRUE

        if (all(isopen)) {
          if (attr(isif, "value") == TRUE) {
            isopen[lvl] <- TRUE
            elseflag[lvl] <- FALSE
          } else {
            isopen[lvl]  <- FALSE
          }
        } else {
          isopen[lvl]  <- FALSE
        }
      } else if (as.logical(iselseif)) {   # Deal with 'ifelse'
        if (all(isopen[seq(1, lvl - 1)])) {
          if (attr(iselseif, "value") == TRUE) {
            isopen[lvl]  <- TRUE
            elseflag[lvl] <- FALSE
          } else {
            isopen[lvl]  <- FALSE
          }
        }
      } else if (is_else(ln)) {   # Deal with 'else'
        if (all(isopen[seq(1, lvl - 1)])) {
          if (length(elseflag) < lvl)
            elseflag[lvl] <- TRUE

          if (elseflag[lvl]) {
            isopen[lvl]  <- TRUE
          } else {
            isopen[lvl]  <- FALSE
          }
        }
      } else if (is_end(ln)) {   # Deal with 'end'
        isopen  <- isopen[lvl * -1] # FALSE
        elseflag <- elseflag[lvl * -1] # TRUE
        lvl <- lvl - 1

        if (lvl < 1) {
          stop(paste0("Unexpected '#%end' on line ", idx, "."))
        }

      } else if (as.logical(isinclude)) {  # Deal with 'include'

        if (all(isopen)) {

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
      } else if (all(isopen)) {   # Deal with normal code

        if (nchar(trimws(ln)) == 0 & ismacro) {
          # Do nothing
          # Eliminate blank lines before macro statements
        } else {
          # If it makes it to this point,
          # replace any macro variables and emit as code

          # Replace macro variables
          nln <- mreplace(ln)

          # Increment output index
          idxO <- idxO + 1

          # Print to console if user requests
          if (debug) {
            cat(paste0("[", sprintf("%4d", idx), "][", sprintf("%4d", idxO), "]: ", nln, "\n"))
            emt <- TRUE
          }

          # Append resolved line to output vector
          ret <- append(ret, nln)
        }
      }
    }

    if (debug & emt == FALSE) {
      cat(paste0("[", sprintf("%4d", idx), "][    ]: ", ln, "\n"))
    }
    emt <- FALSE

    # print(paste0("Line: ", idx, ", Level: ", lvl, ", isopen: ", isopen[lvl], ", elseflag: ", elseflag[lvl]))
    # print(paste0(isopen, collapse = ", "))
    # print(paste0(elseflag, collapse = ", "))
    idx <- idx + 1
  }

  if (lvl > 1) {

    stop("End of file reached with conditional block incomplete. Did you forget an '#%end'?")
  }

  return(ret)

}



# Process macro statements
#' @noRd
mprocess_back <- function(lns) {

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
    ln <- sub_funcs(ln)

    # Identify let statements
    islet <- is_let(ln, isopen[[lvl]])

    if (!islet) {

      # browser()

      # If flag
      if (isopen[[lvl]] == TRUE) {
        isif <-  is_if(ln)
      } else {
        isif <- FALSE
      }

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

    print(paste0("Line: ", idx, ", Level: ", lvl, ", isopen: ", isopen[[lvl]]))
    idx <- idx + 1
  }


  return(ret)

}

mreplace <- function(ln) {

  ret <- ln

  if (nchar(ln) > 0) {
    # browser()

    # print(ls(e))

    # Get variables
    vrs <- ls(e)  # ls(envir = e)

    # Sort by number of characters
    # to avoid variable confounding
    nc <- nchar(vrs)
    vrs <- vrs[order(nc, decreasing = TRUE)]
    # print(vrs)

    if (length(vrs) > 0) {
      for (vr in vrs) {

        # vl <- deparse1(e[[vr]])
        # Get value
        vl <- e[[vr]]

        # Ensure value is suitable for replacement
        if (length(vl) > 1) {

          if (is.vector(vl)) {
            # Deal with vectors
            # Need to be converted to a string suitable for replacement
            if (length(names(vl)) > 0) {
              # browser()
              nms <- names(vl)
              nmstr <- paste0("'", nms, "'")

              if (is.character(vl)) {
                vlstr <- paste0("'", vl, "'")

              } else {
                vlstr <- vl
              }
              vl <- paste0("c(", paste0(nmstr, " = ", vlstr, collapse = ", "), ")")
            } else {
              if (is.character(vl)) {
                vl <- paste0("c('", paste0(vl, collapse = "', '"), "')")
              } else {
                vl <- paste0("c(", paste0(vl, collapse = ', '), ")")
              }
            }
          } else {
            vl <- as.character(vl)

          }
        } else if ("Date" %in% class(vl)) {
          vl <- paste0("as.Date('", vl, "')")
        } else if ("POSIXct" %in% class(vl)) {
          vl <- paste0("as.POSIXct('", vl, "')")
        } else if ("POSIXlt" %in% class(vl)) {
          vl <- paste0("as.POSIXlt('", vl, "')")
        } else if (is.character(vl) == FALSE) {
          vl <- as.character(vl)
        }

        # Perform replacement if valid character value
        if (length(vl) == 1 & is.character(vl) == TRUE) {
          ret <- gsub(vr, vl, ret, fixed = TRUE)
        } else {
          stop(paste0("Macro variable '", vr, "' not valid for text replacement."))
        }
      }
    }
  }

  return(ret)
}

# re1 <- "x."
# sm1 <- c('one', 'two', 'three')
# sm2 <- deparse1(sm1)
# ret <- gsub("x.", sm2, re1, fixed = TRUE)
