#' @title A Macro Pre-processor for 'R' Programs
#'
#' @description The \strong{macro} package contains a function to
#' preprocess R scripts, and output a resolved code file.  Pre-processor
#' commands are implemented as special comments.
#'
#' @details
#' The \strong{macro} package attempts to create a macro language for R that is
#' similar to the SAS® macro language.
#'
#' There is only one function in the package:
#' \code{\link{msource}}.  This function acts as a substitute for the Base R
#' \code{source} function.  The difference is that \code{msource} first runs
#' a pre-processor to resolve macro statements. The resolved macro statements
#' are emitted to a separate file.  The \code{msource} function then sources that
#' file normally.
#'
#' By default, the pre-processor output is written to a temporary location,
#' and deleted when the operation is complete.  If desired, you may also
#' supply a path to save this file to a location of your choosing.
#'
#' @section Macro Commands:
#' Macro commands are implemented as a special form of R comment. The special
#' comments begin with "#%".  For example, a macro assignment is written as
#' \code{#%let a <- 1}.
#'
#' Here are the macro features supported by the system:
#' \itemize{
#'   \item \strong{Macro Comments}: A comment just for macro statements.
#'   \item \strong{Macro Variables}: Initialize macro variables to be used as text replacement tokens.
#'   \item \strong{Macro Conditionals}: Emit code conditionally in pre-processing.
#'   \item \strong{Macro Include}: Insert code from external files into your program.
#'   \item \strong{Built-In Macro Functions}: A small number of essential built-in macro functions.
#'   \item \strong{Macro Do Loops}: Emits a block of code repeatedly.
#'   \item \strong{User-Defined Macro Functions}: Custom macro functions to
#'   enhance reuse and reduce redundancy.
#' }
#' The above features give you a simple yet flexible way to perform meta-programming.
#' In this way, the \strong{macro} package can be useful in several situations,
#' notably for code generation.
#'
#' See the \code{\link{msource}} function documentation for additional details.
#' @name macro
#' @aliases macro-package
#' @keywords internal
"_PACKAGE"


#' @title Addin Function to Run msource()
#' @description
#' This function is exposed to the addin menu to run \code{msource()}
#' interactively. The function will run either the
#' currently selected code, or the currently active program.
#' The function sets the "envir" parameter
#' to the global environment and the "clear" parameter to FALSE
#' to enhance the user experience.  The function
#' takes no parameters and is only used by the addin menu.
#' @return The results of \code{msource}, invisibly.
#' @examples
#' # Called from addin menu
#' # runMSource()
#' @export
runMSource <- function() {

  autoSave()

  res <- tryCatch({msource(envir = globalenv(), echo = TRUE)},
                  warning = function(cond) {
                    print("here!")
                    return(cond)
                    },
                  error = function(cond) {return(cond)})

  ret <- processMessages(res)

  invisible(ret)
}



#' @title Addin Function to Run msource() in Debug Mode
#' @description
#' This function is exposed to the addin menu to run \code{msource()}
#' interactively in debug mode. The function will run either the
#' currently selected code, or the currently active program.
#' On the call to \code{msource}, it sets the "debug" and "symbolgen"
#' parameters to TRUE.  The function also sets the "envir" parameter
#' to the global environment and the "clear" parameter to FALSE
#' to enhance the user experience.  The function
#' takes no parameters and is only used by the addin menu.
#' @return The results of \code{msource}, invisibly.
#' @examples
#' # Called from addin menu
#' # runMSourceDebug()
#' @export
runMSourceDebug <- function() {

  autoSave()

  res <- tryCatch({msource(debug = TRUE, symbolgen = TRUE,
                           envir = globalenv())},
                  warning = function(cond) {
                    print("here!")
                    return(cond)
                    },
                  error = function(cond) { return(cond) })

  ret <- processMessages(res)

  invisible(ret)

}

#' @noRd
processMessages <- function(res) {

  ret <- res

  print(ret)

  if ("warning" %in% class(res)) {

    print(warnings())

  }

  if ("error" %in% class(res)) {

    if (length(res$call) > 1) {
      cl <- res$call[1]
    } else {
      cl <- res$call
    }

    msg <- paste0("Error in ", cl, ": ", res$message, "\n", collapse = "\n")

    cat(msg)
  }

  return(ret)

}

# Deal with autosave choices. Behavior is to automatically save the opened
# document if the user is executing the entire document.  If there are unsaved
# changes, those changes have to be saved because msource() looks at the file
# on disk.  On the other hand, if the user is only selecting a few lines of
# code to execute, there is no need to save the entire document.  Desired behavior
# is similar to RStudio source button.
#' @noRd
autoSave <- function() {

  # Basic requirement
  if (length(find.package('rstudioapi', quiet=TRUE)) > 0) {

    # If user is selecting something, don't save the document
    if (get_selection() == "") {


      if (is.null(options()[["macro.autosave"]]) == FALSE) {

        opt <- options("macro.autosave")

        # Only save if TRUE
        if (all(opt[[1]] == TRUE)) {
          id <- rstudioapi::documentId(FALSE)

          rstudioapi::documentSave(id)
        }

      } else {  # Default is to save document
        id <- rstudioapi::documentId(FALSE)

        rstudioapi::documentSave(id)
      }



    }
  }
}

