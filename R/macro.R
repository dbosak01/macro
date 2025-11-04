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
#' This function is exposed to the addin menu to run \code{msource()}.
#' @return NULL
#' @export
runMSource <- function() {

  msource()

  return(NULL)
}



#' @title Addin Function to Run msource() in Debug Mode
#' @description
#' This function is exposed to the addin menu to run \code{msource()}
#' in debug mode.
#' @return NULL
#' @export
runMSourceDebug <- function() {


  msource(debug = TRUE, symbolgen = TRUE)

  return(NULL)

}


