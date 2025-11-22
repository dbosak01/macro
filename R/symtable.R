
# Macro Symbol Table Functions ----------------------------------------------


#' @title Clear the Macro Symbol Table
#' @description
#' The \code{symclear} function clears the macro symbol table of any
#' stored macro variables and macro functions.  The function is used
#' to avoid contamination between one call to \code{msource} and the
#' next.  It is called automatically when the "clear" parameter of
#' \code{msource} is set to TRUE.  If the "clear" parameter is
#' set to FALSE, you can clear the symbol table manually with
#' the \code{symclear} function.
#' @param variables Whether or not to clear the macro symbol table.
#' Default is TRUE.
#' @param functions Whether or not to clear the macro function list.
#' Default is TRUE.
#' @returns
#' The number of objects cleared, invisibly.  The function also
#' outputs a message saying how many objects were cleared.
#' @examples
#' library(macro)
#'
#' # Get path to demo macro program
#' src <- system.file("extdata/Demo4.R", package = "macro")
#'
#' # Display source code
#' # - This is the macro input code
#' cd <- readLines(src)
#' cat(paste(cd, "\n"))
#' # #% Create some macro variables
#' # #%let x <- 1
#' # #%let y <- 2
#' # #%let z <- &x + &y
#' #
#' # #% Create a macro function
#' # #%macro test(vl = Hello!)
#' # print("&vl")
#' # #%mend
#'
#' # Execute source code
#' msource(src, echo = FALSE)
#'
#' # View symbol table
#' symtable()
#' # # Macro Symbol Table: 3 macro variables
#' #   Name Value
#' # 1   &x     1
#' # 2   &y     2
#' # 3   &z 1 + 2
#' # # Macro Function List: 1 macro functions
#' #   Name Parameter Default
#' # 1 test        vl  Hello!
#'
#' # Clear symbol table
#' symclear()
#' # Clearing macro symbol table...
#' # 4 items cleared.
#'
#' # View symbol table again
#' symtable()
#' # # Macro Symbol Table: (empty)
#' # # Macro Function List: (empty)
#'
#' @family symtable
#' @seealso [msource()]
#' @export
symclear <- function(variables = TRUE, functions = TRUE) {

  cnt <- 0

  if (variables) {
    message("Clearing macro symbol table...\n")

    # Clear out macro environment (symbol table)
    enms <- ls(gbl$env)
    cnt <-  length(enms)
    if (cnt > 0) {
      rm(list = enms, envir = gbl$env)
    }
  }

  if (functions) {
    # Clear out macros
    if (length(gbl$macros) > 0) {
      cnt <- cnt + length(gbl$macros)
      gbl$macros <- list()
    }
  }

  message(paste0(cnt, " items cleared.\n"))


  invisible(cnt)

}

#' @title Examine the Macro Symbol Table
#' @description
#' The \code{symtable} function extracts the contents of the macro symbol table
#' and macro function list.  The symbol table information is returned as an object.
#' The object can be printed or navigated programatically.
#' @returns
#' An object of class "symtable".  The object contains a list of macro symbols
#' and their values.  It also contains a list of macro functions, their parameters,
#' and the associated code.
#' @examples
#' library(macro)
#'
#' # Get path to demo macro program
#' src <- system.file("extdata/Demo4.R", package = "macro")
#'
#' # Display source code
#' # - This is the macro input code
#' cd <- readLines(src)
#' cat(paste(cd, "\n"))
#' # #% Create some macro variables
#' # #%let x <- 1
#' # #%let y <- 2
#' # #%let z <- &x + &y
#' #
#' # #% Create a macro function
#' # #%macro test(vl = Hello!)
#' # print("&vl")
#' # #%mend
#'
#' # Execute source code
#' msource(src, echo = FALSE)
#'
#' # Examine symbol table
#' res <- symtable()
#'
#' # View results
#' print(res)
#' # # Macro Symbol Table: 3 macro variables
#' #   Name Value
#' # 1   &x     1
#' # 2   &y     2
#' # 3   &z 1 + 2
#' # # Macro Function List: 1 macro functions
#' #   Name Parameter Default
#' # 1 test        vl  Hello!
#'
#' # View results structure
#' print(res, verbose = TRUE)
#' # $variables
#' # $variables$`&x`
#' # [1] "1"
#' #
#' # $variables$`&y`
#' # [1] "2"
#' #
#' # $variables$`&z`
#' # [1] "1 + 2"
#' #
#' #
#' # $functions
#' # $functions$test
#' # $functions$test$parameters
#' # $functions$test$parameters$vl
#' # [1] "Hello!"
#' #
#' #
#' # $functions$test$code
#' # [1] "print(\"&vl\")"
#' # attr(,"start")
#' # [1] 8
#' # attr(,"end")
#' # [1] 8
#'
#' @family symtable
#' @seealso [msource()]
#' @export
symtable <- function() {


  ret <- structure(list(), class = c("symtable", "list"))

  enms <- ls(gbl$env)
  sret <- list()
  for (nm in enms) {
    sret[[nm]] <- gbl$env[[nm]]
  }

  mret <- list()
  if (length(gbl$macros) > 0) {
    mret <- gbl$macros
  }

  # Return list of variables and functions
  ret$variables <- sret
  ret$functions <- mret


  return(ret)
}


#' @title Print the Macro Symbol Table
#' @description A class-specific instance of the \code{print} function for
#' a macro symbol table and function list.
#' Use \code{verbose = TRUE} to print the catalog as a list.
#' @param x The format catalog to print.
#' @param ... Any follow-on parameters.
#' @param verbose Whether or not to print the format catalog in verbose style.
#' By default, the parameter is FALSE, meaning to print in tabular style.
#' @return The object, invisibly.
#' @family fcat
#' @examples
#' library(macro)
#'
#' # Get path to demo macro program
#' src <- system.file("extdata/Demo4.R", package = "macro")
#'
#' # Execute source code
#' msource(src, echo = FALSE)
#'
#' # Examine symbol table
#' res <- symtable()
#'
#' # View results
#' print(res)
#' # # Macro Symbol Table: 3 macro variables
#' #   Name Value
#' # 1   &x     1
#' # 2   &y     2
#' # 3   &z 1 + 2
#' # # Macro Function List: 1 macro functions
#' #   Name Parameter Default
#' # 1 test        vl  Hello!
#'
#' # View results structure
#' print(res, verbose = TRUE)
#' # $variables
#' # $variables$`&x`
#' # [1] "1"
#' #
#' # $variables$`&y`
#' # [1] "2"
#' #
#' # $variables$`&z`
#' # [1] "1 + 2"
#' #
#' #
#' # $functions
#' # $functions$test
#' # $functions$test$parameters
#' # $functions$test$parameters$vl
#' # [1] "Hello!"
#' #
#' #
#' # $functions$test$code
#' # [1] "print(\"&vl\")"
#' # attr(,"start")
#' # [1] 8
#' # attr(,"end")
#' # [1] 8
#'
#' @import crayon
#' @family symtable
#' @seealso [msource()]
#' @export
print.symtable <- function(x, ..., verbose = FALSE) {

  if (verbose == TRUE) {

    print(unclass(x))

  } else {

    grey60 <- make_style(grey60 = "#999999")

    if (length(x$variables) == 0) {

      cat(grey60("# Macro Symbol Table: (empty)\n"))

    } else {

      cat(grey60("# Macro Symbol Table: " %+%
                   as.character(length(x$variables)) %+% " macro variables\n"))

      nms <- names(x$variables)

      vls <- c()
      for (nm in nms) {
        vls <- append(vls, paste0(x$variables[[nm]], collapse = ", "))
      }

      sret <- data.frame(Name = nms, Value = vls)

      print(sret)
    }


    if (length(x$functions) == 0) {

      cat(grey60("# Macro Function List: (empty)\n"))

    } else {

      cat(grey60("# Macro Function List: " %+%
                   as.character(length(x$functions)) %+% " functions\n"))

      nms <- names(x$functions)


      for (idx in seq(1, length(nms))) {

        nm <- nms[idx]
        prms <- x$functions[[nm]]$parameters
        pnms <- names(x$functions[[nm]]$parameters)

        cat(grey60(paste0("# Function '%", nm, "': ", as.character(length(prms)),
                          " parameters\n")))

        if (length(pnms) > 0) {
          for (pnm in pnms) {

            if (is.null(prms[[pnm]]) || prms[[pnm]] == "") {
              cat(paste0("- ", pnm, "\n"))
            } else {
              cat(paste0("- ", pnm, " = ", prms[[pnm]], "\n"))
            }
          }
        }
      }
    }
  }

  invisible(x)
}


#' @title Get a Variable Value from the Macro Symbol Table
#' @description
#' The \code{symget} function extracts the value of a single macro variable
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
#' # Set environment variable using symput()
#' symput("env", "prod")
#'
#' # Macro Execute Source Code
#' # - set clear to FALSE to so "env" value is not removed
#' msource(src, echo = FALSE, clear = FALSE)
#'
#' # View "pth" macro variable
#' res <- symget("pth")
#'
#' # View results
#' # - Path is set to the "prod" value
#' res
#' # [1] "/projects/prod/data"
#'
#' @family symtable
#' @seealso [msource()]
#' @export
symget <- function(name) {

  ret <- NA

  enms <- ls(gbl$env)
  nm <- paste0("&", name)
  if (nm %in% enms) {
    ret <- get(nm, envir = gbl$env)
  }

  return(ret)
}



#' @title Assign a Variable in the Macro Symbol Table
#' @description
#' The \code{symput} function assigns the value of a macro
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
#' # Set env macro variable using symput()
#' symput("env", "prod")
#'
#' # Macro Execute Source Code
#' # - set clear to FALSE to so "env" value is not removed
#' msource(src, echo = FALSE, clear = FALSE)
#'
#' # View "pth" macro variable
#' res <- symget("pth")
#'
#' # View results
#' # - Path is set to the "prod" value
#' res
#' # [1] "/projects/prod/data"
#'
#' @family symtable
#' @seealso [msource()]
#' @export
symput <- function(x, value = NULL) {

  nm <- paste0("&", x)

  if (!is.null(value)) {
    vl <- value
    if (!is.character(vl)) {
      vl <- as.character(value)
      if (!is.null(names(value))) {
        names(vl) <- names(value)
      }
    }

    assign(nm, vl, envir = gbl$env)
  } else {
    rm(list = nm, envir = gbl$env)
  }

  invisible(x)
}




