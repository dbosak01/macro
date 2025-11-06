
# Macro Symbol Table Functions ----------------------------------------------


#' @title Macro Clear Function
#' @description
#' The \code{mclear} function clears the macro symbol table of any
#' stored macro variables and macro functions.  The function is used
#' to avoid contamination between one call to \code{msource} and the
#' next.  It is called automatically when the "clear" parameter of
#' \code{msource} is set to TRUE.  The function takes no parameters.
#' @returns
#' The number of objects cleared, invisibly.  The function also
#' outputs a message saying how many objects were cleared.
#' @export
mclear <- function() {

  cat("Clearing macro symbol table...\n")

  # Clear out macro environment (symbol table)
  enms <- ls(gbl$env)
  cnt <-  length(enms)
  if (cnt > 0) {
    rm(list = enms, envir = gbl$env)
  }

  if (length(gbl$macros) > 0) {
    cnt <- cnt + length(gbl$macros)
    gbl$macros <- list()
  }

  cat(paste0(cnt, " items cleared.\n"))

  invisible(cnt)

}

#' @title Macro Put Function
#' @description
#' The \code{mput} function extracts the contents of the macro symbol table
#' as a data frame, and, by default, displays it in the console.
#' The function can be used to examine the symbol table values.
#' @returns
#' A data frame of macro symbols and their values.
#' @export
mput <- function() {

  enms <- ls(gbl$env)
  evl <- c()
  for (nm in enms) {
    evl <- append(evl, gbl$env[[nm]])
  }


  ret <- data.frame("Name" = enms,
                    "Value" = evl)


  return(ret)
}


#' @title Macro Get Function
#' @description
#' The \code{mget} function extracts the value of a single macro variable
#' from the macro symbol table.
#' @param name The name of the macro variable as a quoted string, with no
#' leading ampersand or trailing dot (".").  The leading ampersand will
#' be added automatically by the function.  This
#' parameter is required.
#' @returns
#' The value of the macro variable as a character string.  If the variable
#' name is not found, the function will return an NA.
#' @examples
#' library(macro)
#'
#' # Get path to demo macro program
#' src <- system.file("extdata/Demo3.R", package = "macro")
#'
#' # Create temporary output path
#' tmp <- file.path(tempdir(), "Demo3_mod.R")
#'
#' # Display source code
#' # - This is the macro input code
#' cd <- readLines(src)
#' cat(paste(cd, "\n"))
#' # #% Determine appropriate data path
#' # #%if ("&env." == "prod")
#' #   #%let pth <- /projects/prod/data
#' # #%else
#' #   #%let pth <- /projects/dev/data
#' # #%end
#'
#' # Set environment variable using mset()
#' mset("env", "prod")
#'
#' # Macro Execute Source Code
#' # - set clear to FALSE to so "env" value is not removed
#' msource(src, tmp, clear = FALSE)
#'
#' # View "pth" macro variable
#' res <- mget("pth")
#'
#' # View results
#' res
#' # [1] "/projects/prod/data"
#'
#' @export
mget <- function(name) {

  ret <- NA

  enms <- ls(gbl$env)
  nm <- paste0("&", name)
  if (nm %in% enms) {
    ret <- get(nm, envir = gbl$env)
  }

  return(ret)
}



#' @title Macro Set Function
#' @description
#' The \code{mset} function assigns the value of a macro
#' variable from regular R code.
#' @param x The name of the macro variable to assign,
#' passed as a quoted string with no leading ampersand or trailing
#' dot (".").  The leading ampersand will be added automatically
#' by the function.  This parameter is required.
#' @param value The value of the macro variable to assign.
#' Value will be converted to a character string. This parameter is
#' not required.  If the value parameter is not supplied, the variable
#' will be removed from the symbol table.
#' @returns
#' The macro name, invisibly.
#' @examples
#' library(macro)
#'
#' # Get path to demo macro program
#' src <- system.file("extdata/Demo3.R", package = "macro")
#'
#' # Create temporary output path
#' tmp <- file.path(tempdir(), "Demo3_mod.R")
#'
#' # Display source code
#' # - This is the macro input code
#' cd <- readLines(src)
#' cat(paste(cd, "\n"))
#' # #% Determine appropriate data path
#' # #%if ("&env." == "prod")
#' #   #%let pth <- /projects/prod/data
#' # #%else
#' #   #%let pth <- /projects/dev/data
#' # #%end
#'
#' # Set environment variable using mset()
#' mset("env", "prod")
#'
#' # Macro Execute Source Code
#' # - set clear to FALSE to so "env" value is not removed
#' msource(src, tmp, clear = FALSE)
#'
#' # View "pth" macro variable
#' res <- mget("pth")
#'
#' # View results
#' res
#' # [1] "/projects/prod/data"
#'
#' @export
mset <- function(x, value = NULL) {

  nm <- paste0("&", x)

  if (!is.null(value)) {
    assign(nm, as.character(value), envir = gbl$env)
  } else {
    rm(nm, envir = gbl$env)
  }

  invisible(x)
}




