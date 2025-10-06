
# Globals -----------------------------------------------------------------


# Macro symbol table
e <- new.env()

# Global internal variables
gbl <- new.env()

# Local execution environment
lcl <- new.env()

# Separator for debug output
strs <- paste0(rep("*", 80), collapse = "")



# Macro Source Function ---------------------------------------------------



#' @title Pre-process and Source
#' @description
#' The \code{msource} function is used to pre-process and source macro-enabled
#' programs. The function first runs the macro pre-processor to evaluate
#' any macro commands. During macro evaluation, an output file is created
#' that contains generated code. After pre-processing, this generated code
#' is sourced normally.
#' @details
#' R does not have a native macro language.  The \strong{macro} package
#' attempts to make up for that deficiency.  The package devised a set
#' of macro commands inspired by SAS syntax, which can be added to any R script.
#' The macro commands are placed in R comments, prefixed by the characters
#' "#%".
#'
#' The macro commands function as pre-processor directives, and the
#' \code{msource} function is the pre-processor.  These commands operate as
#' text replacement and branching functions.  They allow you to perform
#' high-level manipulation of your program before the code is executed.
#'
#' @section How to Use:
#' The \code{msource} function works very much like the Base R \code{source}
#' function.  You pass the path to the code file as the first parameter, and
#' \code{msource} will run it.  The difference is that \code{msource} will
#' first pre-process the file and resolve any macro commands. The resolved
#' code is placed by default into a temp file and then executed.  If you wish
#' to save the generated code, supply a local path on the \code{file_out}
#' parameter.
#'
#' The \code{msource} function can be run on the command line or from an R script.
#' When run from the command line, the function will take the currently active
#' program in RStudio as the default input.  That means if you are working
#' interactively in RStudio, you can easily execute your macro code just by
#' running \code{msource()} on the command line with no parameters.
#'
#' @section Macro Commands:
#' Here is a summary of the available macro commands:
#' \itemize{
#'   \item{\strong{#%&lt;comment&gt;}: A macro comment.}
#'   \item{\strong{#%let &lt;variable&gt; <- &lt;value&gt;}: Declares a macro variable
#'   and assigns it a value.}
#'   \item{\strong{#%include '&lt;path&gt;'}: Inserts code from included file as text
#'   into current program.}
#'   \item{\strong{#%if (&lt;condition&gt;)}: Begins a macro conditional block.}
#'   \item{\strong{#%elseif (&lt;condition&gt;)}: Defines a subsequent conditional block.}
#'   \item{\strong{#%else}: Identifies the default behavior in a condition.}
#'   \item{\strong{#%end}: Ends a macro condition.}
#'   \item{\strong{#%do &lt;variable&gt; = &lt;start&gt; %to &lt;end&gt;}: Defines a
#'   macro do loop block.}
#'   \item{\strong{%sysfunc(&lt;expression&gt;)}: Evaluates an R expression as part of
#'   a macro command.}
#'   \item{\strong{%symexist(&lt;name&gt;)}: Determines if a macro variable name
#'   exists in the macro symbol table.}
#'   \item{\strong{%symput(&lt;expression&gt;)}: Assigns the result of
#'   an expression in the execution environment to a macro variable.}
#' }
#'
#' Note that there are no user-defined macro functions in the R macro language.
#' Use regular R function definitions instead.
#'
#' You can find extensive documentation for the above macro commands in the
#' the Macro Language vignette. To access the vignette, run
#' \code{vignette("macro-language")} on the R command line.
#'
#' @section Pre-Processor:
#' There are three main steps to processing a macro-enabled program: pre-process
#' the input file, generate an output file, and then execute the
#' output file.
#'
#' The pre-processor works by inputting each line of the program line by line.
#' For each line in the input
#' script, the function will assign and replace any macro variables. The pre-processor
#' also evaluates any macro conditions.  For any macro conditions that are
#' TRUE, the pre-processor will output that line to the generated code file.
#' If a condition evaluates as FALSE, the lines inside that block will
#' be ignored.
#'
#' In short, the pre-processor scans the input program from top to bottom,
#' spitting out lines or not depending on the macro conditions.  This
#' logic makes the \strong{macro} package perfect for code generation.
#'
#' @section Code Generation:
#' Code generation in R is most often performed using string concatenation, and
#' writing out strings to a file.  The \strong{macro} package gives you
#' a much easier way to do it. Using pre-processor directives, you can
#' write your code as normal code.  These code lines will be subject to the
#' syntax checker, and any errors in syntax will be highlighted immediately.
#'
#' The \strong{macro} package also makes it easy to construct code from
#' code snippets.  You can store your snippets in separate files, and
#' then pull them together using \code{#%include} and macro logic.
#'
#' The collation process is further enhanced by the \strong{macro} debugger.
#' The debugger allows you to solve issues in the macro code much
#' faster and easier than doing string concatenation.
#'
#' @section Debugger:
#' The \code{msource} function has a built-in debugger.  The debugger can be
#' very useful when identifying problems in your macro-enabled program.  The
#' debugger can be activated by setting \code{debug = TRUE} on your call
#' to \code{msource}.  When activated, the debugger will by default send
#' debug information to the R console. The debug information can show you
#' which lines made it into the output file, and how those lines resolved.
#' It will also echo the source call for the generated code. If an error occurs
#' at either of these stages, the debug information will help you pinpoint
#' which line produced the error.
#'
#' For a full explanation of the debugger capabilities and several examples,
#' see the debugging vignette at \code{vignette('macro-debug')}.
#'
#' @section Output File Execution:
#' Once the output file has been generated successfully, the \code{msource}
#' function will execute it normally using the Base R \code{source} function.
#' At this point the generated code runs like a normal R program, and any
#' errors or warnings will be sent to the console by default.
#'
#' If you do not wish to execute the generated code, use the \code{exec} parameter
#' to turn off execution.
#'
#' @param pth The path to the R program to process. This parameter is required.
#' It will default to the currently activated program in the development environment.
#' @param file_out If you want to save or view the generated code
#' from the \code{msource} function, supply a full path and
#' file name on this parameter. Default is NULL. When NULL,
#' the function will create a temp file for the generated code. The temp
#' file will be deleted when processing is complete.
#' @param envir The environment to be used for program execution.
#' Default is the parent frame.
#' @param exec Whether or not to execute the output file after pre-processing.
#' Default is TRUE. When FALSE, only the pre-processing step is performed.
#' If a \code{file_out} parameter is supplied, the generated code file
#' will still be created.
#' @param debug If TRUE, prints lines to the console as they are processed.
#' This information case be useful for debugging macro code.
#' Default is FALSE.
#' @param debug_out A path to a file to be used for debugging.  If a path
#' is supplied, debug output will be written to the file instead of the
#' console. Default is NULL.
#' @param ... Follow-on parameters to the \code{source} function. See
#' the \code{\link{source}} function for additional information.
#' @import common
#' @importFrom utils capture.output
#' @returns The results of the \code{source()} function, invisibly.  The path
#' of the resolved output file is also included under the "$output" list item.
#' @examples
#'
#' # Create temp file names
#' tmp <- file.path(tempdir(), "test.R")
#' out <- file.path(tempdir(), "test_mod.R")
#'
#' # Write macro code to temp file
#' cat("#%let a <- 1\n", file = tmp)
#' cat("#%if (a. == 1)\n", file = tmp, append = TRUE)
#' cat("print('a is one!')\n", file =tmp, append = TRUE)
#' cat("#%else\n", file = tmp, append = TRUE)
#' cat("print('a is something else!')\n", file = tmp, append = TRUE)
#' cat("#%end", file = tmp, append = TRUE)
#' cat("\n", file = tmp, append = TRUE)
#'
#' # Process temp file
#' res <- msource(tmp, out)
#' # [1] "a is one!"
#'
#' # Read output file
#' readLines(res$output)
#' # [1] "print('a is one!')"
#'
#' # Rerun in debugger
#' msource(tmp, out, debug = TRUE)
#' # ********************************************************************************
#' # **  Pre-Processing
#' # ********************************************************************************
#' # -    File In: C:\Users\dbosa\AppData\Local\Temp\RtmpyM9Kep/test.R
#' # -   File Out: C:\Users\dbosa\AppData\Local\Temp\RtmpyM9Kep/test_mod.R
#' # ********************************************************************************
#' # [ In#][Out#]:
#' # [   1][    ]: #%let a <- 1
#' # [   2][    ]: #%if (a. == 1)
#' # [   3][   1]: print('a is one!')
#' # [   4][    ]: #%else
#' # [   5][    ]: print('a is something else!')
#' # [   6][    ]: #%end
#' # ********************************************************************************
#' # **  Execution
#' # ********************************************************************************
#' #
#' # > print('a is one!')
#' # [1] "a is one!"
#' #
#' # ********************************************************************************
#' # **  End
#' # ********************************************************************************
#'
#' @export
msource <- function(pth = Sys.path(), file_out = NULL, envir = parent.frame(),
                    exec = TRUE, debug = FALSE, debug_out = NULL,
                    ...) {

  #browser()

  if (!file.exists(pth)) {
    stop(paste0("File '", pth, "' not found."))
  }

  if (is.null(envir)) {
    stop("Environment cannot be null.")
  } else {
    e <- envir
  }

  # To write cat() to the console,
  # requires empty string.
  # NULL will error.
  if (is.null(debug_out)) {
    debug_out <- ""
  }

  # Set globally no matter what
  gbl$debug_out <- debug_out

  if (debug) {

    # If debug_out is requested
    # Write errors to log
    if (nchar(debug_out) > 0) {
      # Attach error event handler
      options(error = log_error)
    }

    # Write log header
    log_debug(strs, appnd = FALSE)
    log_debug("**  Pre-Processing")
    log_debug(strs)
    log_debug(paste0("-    File In: ", pth))
  }

  # Stage 1: Run pre-process routine
  ppth <- preprocess(pth, file_out, envir, debug, debug_out)

  # Stage 2: Source pre-process output
  if (exec) {
    if (debug) {

      # Write execution header
      log_debug(strs)
      log_debug("**  Execution")
      log_debug(strs)

      if (debug_out == "") {
        # Execute source with echo
        ret <- source(ppth, local = e, echo = TRUE, ...)
      } else {
        # Write source echo to log
        capture.output(source(ppth, local = e, echo = TRUE, ...),
                       file = debug_out, append = TRUE,
                       type = c("output", "message"))
        ret <- list(invisible = TRUE, value = NA)

      }
    } else {
      # Execute source normally
      ret <- source(ppth, local = e, ...)
    }
  } else {
    # If no execution, create list return value
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
    # Write log end indicator
    log_debug("")
    log_debug(strs)
    log_debug("**  End")
    log_debug(strs)

    # Release error sink
    if (nchar(debug_out) > 0) {
      # Attach error event handler
      options(error = NULL)
    }
  }

  # Source return value is ugly, so hide it
  invisible(ret)
}

# Input macro code and output path to resolved code
#' @noRd
preprocess <- function(pth, file_out, envir, debug, debug_out) {

  # Create output file name
  if (is.null(file_out)) {
    bs <- basename(pth)
    npth <- file.path(tempdir(), bs)
  } else {
    npth <- file_out
  }

  # print(npth)
  if (debug) {
    log_debug(paste0("-   File Out: ", npth))
    log_debug(strs)
    log_debug("[ In#][Out#]:")
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

  # Clear out local environment
  lnms <- ls(lcl)
  if (length(lnms) > 0) {
    rm(list = lnms, envir = lcl)
  }

  # Process lines
  nlns <- mprocess(lns, debug, debug_out)

  # Output program
  fl2 <- file(npth, open = "w", encoding = "native.enc")
  writeLines(nlns, con = fl2, useBytes = TRUE)
  close(fl2)

  ret <- npth

  return(ret)
}


# Process macro statements
#' @noRd
mprocess <- function(lns, debug, debug_out) {

  ret <- c()

  # print(ls(e))

  lvl <- 1 # lvl 1 is open code
  isopen <- c()
  isopen[lvl] <- TRUE
  elseflag <- c()
  elseflag[lvl] <- TRUE
  idx <- 1 # Input index
  idxO <- 0 # Output index
  lncnt <- length(lns)
  emt <- FALSE
  gbl$buffer <- c()  # execution buffer

  while (idx <= lncnt) {

    # Initialize line
    ln <- lns[idx]

    # If gets mismatched
    if (is.na(ln)) {
      break
    }

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

      # Do flag
      isdo <- is_do(ln)

      # Next line macro
      if (idx < lncnt) {
        nl <- lns[idx + 1]
        if (is.na(nl)) {
            ismacro <- FALSE
          } else {

          if (is_comment(nl) | trimws(nl) == "") {
            ismacro <- TRUE
          } else {
            ismacro <- FALSE
          }
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
          lns <- c(lns[seq(1, idx)], nlns, lns[seq(idx + 1, lncnt)])

          # Reset line count
          lncnt <- length(lns)

          # Resolve variables for better debug output
          ln <- mreplace(ln)

        }
      } else if (as.logical(isdo)) {  # Deal with 'do'

        if (all(isopen)) {

          # Process do and return do_info
          nlns <- process_do(lns, idx, lvl, isdo)

          # Get ending point of do block
          doend <- attr(nlns, "end")

          # Insert into lns vector
          lns <- c(lns[seq(1, idx)], nlns, lns[seq(doend + 1, lncnt)])

          # Reset line count
          lncnt <- length(lns)

          # Resolve variables for better debug output
          ln <- mreplace(ln)

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
            log_debug(paste0("[", sprintf("%4d", idx), "][", sprintf("%4d", idxO), "]: ", nln))
            emt <- TRUE
          }

          # Append to buffer for %symput
          gbl$buffer <- append(gbl$buffer, nln)

          # Append resolved line to output vector
          ret <- append(ret, nln)
        }
      }
    }

    if (debug & emt == FALSE) {
        log_debug(paste0("[", sprintf("%4d", idx), "][    ]: ", ln))
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

# Replace macro variables
#' @noRd
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

      # Iterate up to 10 times for possible nested replacements
      for (itr in seq(1, 10)) {

        # Get vector of variables to replace
        fvrs <- c()
        for (vr in vrs) {
          if (grepl(vr, ln, fixed = TRUE)[1]) {
            fvrs <- append(fvrs, vr)
          }
        }

        if (length(fvrs) == 0) {
          # Bail if nothing found
          break
        } else {

          # Do replacements for found variables
          for (vr in fvrs) {

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
        # Update ln for next iteration
        ln <- ret
      }
    }
  }

  return(ret)
}




