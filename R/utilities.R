

# Detection Utilities -----------------------------------------------------

# Determines if line is a #%let and, if so, make assignment
# into symbol table.
#' @noRd
is_let <- function(ln, opn = TRUE) {

  ret <- FALSE

  # Look for #%let
  dtct <- grepl("#%let", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    if (opn) {
      # Remove %let keyword
      nl <- trimws(sub("#%let", "", ln, fixed = TRUE))

      spl <- c()

      # Assignment operator can be <- or =
      if (grepl("<-", nl, fixed = TRUE)[[1]]) {
        # Split on first <- found
        pos <- regexpr("<-", nl, fixed = TRUE)[[1]]
        spl[1] <- substring(nl, 1, pos - 1)
        spl[2] <- substring(nl, pos + 2)
      } else if (grepl("=", nl, fixed = TRUE)[[1]]) {
        # Replace first = with <- to deal with vectors that contain =
        nl <- sub("=", "<-", nl, fixed = TRUE)
        pos <- regexpr("<-", nl, fixed = TRUE)[[1]]
        spl[1] <- substring(nl, 1, pos - 1)
        spl[2] <- substring(nl, pos + 2)
      } else {
        spl <- nl
      }

      if (length(spl) > 1) {

        # Deal with %symput
        has_symput <- grepl("%symput", spl[2], fixed = TRUE)[1]
        if (has_symput) {

          # Buffer collects lines since beginning of program
          # or last %symput evaluation.  Code below manages it.

          # Collapse buffer
          buffcd <- paste0(gbl$buffer, collapse = "\n")

          # Evaluate buffer
          res <- tryCatch({eval(str2expression(buffcd), envir = lcl)
                           TRUE},
                          error = function(cond) {
                            NULL
                          })

          # Prepare to eval
          nvl <- sub("%symput", "", spl[2], fixed = TRUE)
          nvl <- trimws(mreplace(nvl))

          # Evaluate expression
          vl <- tryCatch({eval(str2expression(nvl), envir = lcl)},
                         error = function(cond) { return(cond) })

          if ("error" %in% class(vl)) {
            msg <- paste0("Failed to evaluate\nmacro %symput expression: '",
                          nvl, "'\n", "-Message: ", vl$message)
            stop(msg)
          }

          # assign(paste0(trimws(spl[1]), "."), vl, envir = gbl$env)
          assign(paste0("&", trimws(spl[1])), vl, envir = gbl$env)

          # Clear out buffer if execution was successful
          if (!is.null(res)) {
            gbl$buffer <- c()
          }

        } else {

          # Assign value to macro variable
          vl <- trimws(mreplace(spl[2]))
          # assign(paste0(trimws(spl[1]), "."), vl, envir = gbl$env)
          assign(paste0("&", trimws(spl[1])), vl, envir = gbl$env)
        }

      } else {

        # Clear out macro variable
        # assign(paste0(trimws(spl), "."), NULL, envir = gbl$env)
        assign(paste0("&", trimws(spl)), NULL, envir = gbl$env)
      }
    }
  }

  return(ret)
}

# This function deals with sysfunc and symexist
#' @noRd
#' @import fmtr
sub_funcs <- function(ln) {

  # browser()

  ret <- ln

  # Look for %sysfunc
  pos <- regexpr("%sysfunc(", ln, fixed = TRUE)[[1]]
  if (pos > 0) {

    spos <- pos + 8
    tmp <- substring(ln, spos)
    epos <- nchar(ln)
    splt <- strsplit(tmp, "", fixed = TRUE)[[1]]
    open <- 0
    sysex <- ""   # expression
    idx <- spos
    cma <- NA
    fmt <- NA

    # Have to traverse character by character
    for (chr in splt) {
      if (chr == "(") {
        open <- open + 1
      }
      if (chr == ")") {
        open <- open - 1
      }
      if (chr == "," & open == 1) {

        cma <- idx
      }
      if (open == 0) {
        epos <- idx
        if (is.na(cma)) {

          sysex <- substring(ln, spos + 1, epos - 1)
        } else {
          sysex <- substring(ln, spos + 1, cma - 1)
          fmt <- trimws(substring(ln, cma + 1, epos - 1))
        }
        break
      }
      idx <- idx + 1
    }
    if (sysex != "") {

      nsysex <- mreplace(sysex)

      # Evaluate expression
      tres <- tryCatch({eval(str2expression(nsysex), envir = gbl$env)},
                       error = function(cond) { return(cond) })

      if ("error" %in% class(tres)) {
        msg <- paste0("Failed to evaluate \n%sysfunc expression: '",
                      nsysex, "'\n", "-Message: ", tres$message)
        stop(msg)
      }

      # Format if requested
      if (!is.na(fmt)) {

         tres <- fapply(tres, fmt)
      }
      # Return evaluated result in context
      ret <- paste0(substring(ln, 1, pos -1),
                    as.character(tres),
                    substring(ln, epos + 1))
    }
  }

  # Look for %symexist
  pos2 <- regexpr("%symexist(", ret, fixed = TRUE)[[1]]
  if (pos2 > 0) {

    spos <- pos2 + 9
    tmp <- substring(ret, spos)
    epos <- nchar(ret)
    splt <- strsplit(tmp, "", fixed = TRUE)[[1]]
    open <- 0
    sysex <- ""
    idx <- spos

    # Traverse character by character
    for (chr in splt) {
      if (chr == "(") {
        open <- open + 1
      }
      if (chr == ")") {
        open <- open - 1
      }
      if (open == 0) {
        epos <- idx
        sysex <- substring(ret, spos + 1, epos - 1)
        break
      }
      idx <- idx + 1
    }
    if (sysex != "") {

      # Make sure it exists in symbol table
      tres <- exists(paste0("&", sysex), envir = gbl$env)

      # Return in context
      ret <- paste0(substring(ret, 1, pos2 -1),
                    as.character(tres),
                    substring(ret, epos + 1))
    }
  }

  return(ret)
}

#' @noRd
is_if <- function(ln, evaluate = TRUE) {

  ret <- FALSE
  # browser()

  # Look for #%if
  dtct <- grepl("#%if", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    if (evaluate) {

      # Remove #%if text to isolation condition
      nl <- trimws(sub("#%if", "", ln, fixed = TRUE))

      # Resolve any macro variables in condition
      nr <- mreplace(nl)

      # Evaluate condition
      vl <- tryCatch({eval(str2expression(trimws(nr)), envir = gbl$env)},
                     error = function(cond) { cond })

      if ("error" %in% class(vl)) {
        msg <- paste0("Failed to evaluate\nmacro %if condition: '",
                      nr, "'\n", "-Message: ", vl$message)
        stop(msg)
      }

    } else {

      vl <- NA
    }

    # Return evaluation as attribute
    attr(ret, "value") <- vl
  }

  return(ret)
}


#' @noRd
is_end <- function(ln) {

  ret <- FALSE

  # Look for #%end.
  # No way of knowing whether it is an if block end
  # or a do block end.  Calling function will make that determination.
  dtct <- grepl("#%end", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE
  }

  return(ret)
}

# Collapsed "else" and "if" into one word to make
# parsing easier.
#' @noRd
is_elseif <- function(ln) {

  ret <- FALSE

  # Look for #%elseif
  dtct <- grepl("#%elseif", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    nl <- trimws(sub("#%elseif", "", ln, fixed = TRUE))

    nr <- mreplace(nl)

    vl <- tryCatch({eval(str2expression(trimws(nr)), envir = gbl$env)},
                   error = function(cond) { return(cond) })

    if ("error" %in% class(vl)) {
      msg <- paste0("Failed to evaluate\nmacro %elseif condition: '",
                    nr, "'\n", "-Message: ", vl$message)
      stop(msg)
    }

    attr(ret, "value") <- vl
  }

  return(ret)
}

#' @noRd
is_else <- function(ln) {

  ret <- FALSE

  dtct <- grepl("#%else", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE
  }

  return(ret)
}

#' @noRd
is_comment <- function(ln) {

  ret <- FALSE

  # Look for #%
  # Does not necessarily mean it is a real comment.
  # Other choices have to be eliminated.
  # Calling function will perform the elimination.
  dtct <- grepl("#%", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE
  }

  return(ret)
}


#' @noRd
is_include <- function(ln) {

  ret <- FALSE

  # Look for #%include
  dtct <- grepl("#%include ", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    # Isolate path
    nl <- trimws(sub("#%include ", "", ln, fixed = TRUE))
    nl <- gsub('^"|"$', '', nl)
    nl <- gsub("^'|'$", "", nl)

    attr(ret, "path") <- nl
  }

  return(ret)
}

#' @noRd
get_include <- function(pth) {


  if (!file.exists(pth)) {

    stop(paste0("Include file '", pth, "' not found."))

  }

  # Open path and pull out lines
  f <- file(pth, encoding = "UTF-8")

  ret <- readLines(f, warn = FALSE)

  close(f)

  return(ret)

}

#' @noRd
is_do <- function(ln) {

  ret <- FALSE

  dtct <- grepl("#%do ", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    # Resolve any macro variables
    ln <- mreplace(ln)

    # Remove do token
    nl <- trimws(sub("#%do ", "", ln, fixed = TRUE))

    # Split on equals sign
    snl <- strsplit(nl, "=", fixed = TRUE)[[1]]

    # Split on %to
    vr <- trimws(snl[1])
    cnl <- strsplit(snl[2], "%to", fixed = TRUE)[[1]]

    # Get starting number
    strt <- tryCatch({eval(str2expression(trimws(cnl[1])), envir = gbl$env)},
                     error = function(cond) {NA})

    # Get ending number
    end <- tryCatch({eval(str2expression(trimws(cnl[2])), envir = gbl$env)},
                     error = function(cond) {NA})

    # If messed up, give a good message
    if (is.na(strt) | is.na(end)) {
      msg <- paste0("Macro %do loop improperly formed:\n",
                    "-Variable: ", vr, "\n",
                    "-From: ", trimws(cnl[1]), "\n",
                    "-To: ", trimws(cnl[2]))
      stop(msg)
    }

    attr(ret, "variable") <- vr
    attr(ret, "start") <- strt
    attr(ret, "end") <- end
  }


  return(ret)

}

#' @noRd
process_do <- function(lns, idx, lvl, ido) {

  lnstart <- idx + 1
  lnend <- length(lns)
  ret <- c()
  dlvl <- lvl + 1

  # Scan remaining lines for end of do block
  for (ix in seq(lnstart, lnend)) {

    ln <- lns[ix]

    # Look for if
    if (as.logical(is_if(ln, FALSE))) {
      dlvl <- dlvl + 1
    }

    # Look for do
    if (as.logical(is_do(ln))) {
      dlvl <- dlvl + 1
    }

    if (is_end(ln)) {
      dlvl <- dlvl - 1
    }

    if (dlvl == lvl) {
      lnend <- ix - 1
      break
    }

  }

  # Throw error if no end
  if (lnend == length(lns) & dlvl != lvl) {
    stop(paste0("Macro do loop on line ", idx, " not closed.  Did you forget an '#%end'?"))
  }

  # Get do lines
  tmp <- lns[seq(lnstart, lnend)]

  # Create new block
  for (lp in seq(attr(ido, "start"), attr(ido, "end"))) {

    # Set do macro variable
    fln <- paste0("#%let ", attr(ido, "variable"), " <- ", lp)
    ret <- append(ret, fln)

    # Append do lines
    ret <- append(ret, tmp)

  }

  ret <- append(ret, "#% end do")

  attr(ret, "end") <- lnend + 1

  return(ret)
}

# This function performs macro variable substitution,
# taking into account the optional dot and the backticks.
#' @noRd
sub_mvar <- function(ln, varnm, varvl) {

  ret <- ln

  cnt <- gregexpr(varnm, ret, fixed = TRUE)[[1]]

  if (length(cnt) > 0) {
    for (itr in seq(1, length(cnt))) {

      rvl <- varnm
      pos <- regexpr(rvl, ret, fixed = TRUE)
      epos <- pos + nchar(rvl) - 1
      ftick <- FALSE

      if (pos > 1) {

        # Find end of ampersands
        for (idx in seq(1, 9)) {
           if (substring(ret, pos - idx, pos - idx) != "&") {
             if (idx > 1) {
               rvl <- paste0("&", rvl)
               pos <- pos - idx + 1
             }
             break
           }
        }

        # Identify leading tick
        if (substring(ret, pos - 1, pos - 1) == "`") {
          ftick <- TRUE
        }
      }

      if (epos < nchar(ret)) {

        # Replace trailing dot if found
        if (substring(ret, epos, epos) != ".") {
          if (substring(ret, epos + 1, epos + 1) == ".") {
            rvl <- paste0(rvl, ".")
          }
        }

        # Replace trailing tick if found
        if (substring(ret, epos + 1, epos + 1) == "`") {
          if (ftick) {
            rvl <- paste0("`", rvl, "`")
          }
        }
      }

      if (epos + 1 < nchar(ret)) {

        # Replace trailing tick when no trailing dot
        if (substring(ret, epos + 2, epos + 2) == "`") {
          if (ftick) {
            rvl <- paste0("`", rvl, "`")
          }
        }
      }

      # Perform substitution
      ret <- sub(rvl, varvl, ret, fixed = TRUE)

    }
  }

  return(ret)

}

# This function deals with delayed variable resolution.
# Looks and sees if number of ampersands is equal to the
# number of iterations.
#' @noRd
sub_ready <- function(ln, varnm, itr) {

  ret <- TRUE
  pos <- regexpr(varnm, ln, fixed = TRUE)
  ofst <- itr

  # If there are more ampersands than the iteration count, return FALSE
  if (pos > ofst) {
    if (substring(ln, pos - ofst, pos - ofst) == "&") {
      ret <- FALSE
    }
  }

 return(ret)

}

# Macro Utilities ---------------------------------------------------------


# Beginning of macro definition
#' @noRd
is_macro <- function(ln) {

  ret <- FALSE

  # Look for #%macro
  dtct <- grepl("#%macro ", ln, fixed = TRUE)[[1]]
  if (dtct) {

    ret <- TRUE

    # Remove macro keyword
    nl <- trimws(sub("#%macro ", "", ln, fixed = TRUE)[[1]])

    # Get first paren
    pos <- regexpr("(", nl, fixed = TRUE)[[1]]

    # Get macro name
    nm <- substring(nl, 1, pos - 1)

    attr(ret, "name") <- nm
    attr(ret, "parameters") <- get_parms(substring(nl, pos), nm)
  }

  return(ret)
}

# End of macro definition
#' @noRd
is_mend <- function(ln) {

  ret <- FALSE

  # Look for #%mend
  dtct <- grepl("#%mend", ln, fixed = TRUE)[[1]]
  if (dtct) {

    ret <- TRUE

    # Get macro name
    nm <- trimws(sub("#%mend", "", ln, fixed = TRUE))

    # Assign to attribute
    attr(ret, "name") <- nm
  }

  return(ret)
}


# Used for both macro definitions and calls
# def = TRUE is a definition
#' @noRd
get_parms <- function(ln, nm, def = TRUE) {

  nl <- trimws(ln)

  # Get starting paren
  if (substring(nl, 1, 1) == "(") {
    pos <- 1
  } else {
    pos <- regexpr("(", nl, fixed = TRUE)[[1]]
    if (pos < 1) {
      stop(paste0("Macro definition for '", nm, "' malformed."))
    }
  }

  # Get ending paren
  if (substring(nl, nchar(nl), nchar(nl)) == ")") {
    epos <- nchar(nl)
  } else {

    # Reverse string
    nl_rev <- paste(rev(strsplit(nl, NULL)[[1]]), collapse = "")

    # Search from right
    ppos <- regexpr(")", nl_rev, fixed = TRUE)[[1]]
    if (ppos <= 0) {
      stop(paste0("Syntax error on call to macro '", nm, "'"))
    }

    # Get end position
    epos <- nchar(nl) - ppos

  }

  # Get parameter string
  prms <- substring(nl, pos + 1, epos - 1)

  # Break into characters
  sprms <- strsplit(prms, "")[[1]]

  # Return list
  ret <- list()  # Return list of named values
  lvl <- 1    # Level indicator
  pnm <- c()  # Parameter name vector
  vl <- c()   # Value vector
  nmflg <- TRUE  # If on the left side of equals sign

  # Loop through parameter string character by character
  for (ch in sprms) {
    if (lvl == 1) {

      # There can be nested functions in parameter string
      # If encountered, increase the level
      if (ch == "(") {
        lvl <- lvl + 1

        if (nmflg) {
          pnm <- append(pnm, ch)
        } else {
          vl <- append(vl, ch)
        }

      }  else if (ch == "=") {   # equals sign is separator between name and value

        nmflg <- FALSE
        pnm <- trimws(paste0(pnm, collapse = ""))

      } else if (ch == ",") {  # Comma starts new parameter

        nmflg <- TRUE

        # At this point, name is done and can be concatenated
        pnm <- trimws(paste0(pnm, collapse = ""))
        ret[[pnm]] <- trimws(paste0(vl, collapse = ""))

        # Reset name and value vectors
        pnm <- c()
        vl <- c()

      } else {
        if (nmflg) {
          pnm <- append(pnm, ch)
        } else {
          vl <- append(vl, ch)
        }
      }
    } else {  # Deal with nested functions
      if (ch == "(") {
        lvl <- lvl + 1
      } else if (ch == ")") {
        lvl <- lvl - 1
      }

      # Append character appropriately
      if (nmflg) {
        pnm <- append(pnm, ch)
      } else {
        vl <- append(vl, ch)
      }
    }
  }

  # Deal with last parameter
  if (length(pnm) > 0) {
    pnm <- trimws(paste0(pnm, collapse = ""))
    ret[[pnm]] <- trimws(paste(vl, collapse = ""))
  }

  # Macro Call parameters may be positional.
  # Swap name for value if needed.
  if (def == FALSE) {
    if (length(ret) > 0) {
      nms <-  names(ret)
      for (idx in seq(1, length(ret))) {
        if (ret[[idx]] == "") {
          vl <- nms[idx]
          ret[[idx]] <- vl
          names(ret)[idx] <- ""
        }
      }
    }
  }

  return(ret)
}


# Gets code for a macro definition
#' @noRd
get_macro_code <- function(lns, idx, imacro) {

  ret <- c()
  lvl <- 1         # level indicator
  bdx <- idx + 1   # Code begin line index
  edx <- length(lns) # Code ending line index

  # Scan all remaining lines
  for (sdx in seq(bdx, length(lns))) {

    # Get a line
    ln <- lns[sdx]

    # Determine if line is a macro call
    # This is necessary because there can
    # be nested macro definitions
    ismacro <- is_macro(ln)

    if (as.logical(ismacro)) {
      lvl <- lvl + 1
    } else if (as.logical(is_mend(ln))) {
      lvl <- lvl - 1

      # Only reach end if levels are lined up
      if (lvl == 0) {
        edx <- sdx - 1
        break
      }
    }

    # Otherwise, append the line to the output
    ret <- append(ret, ln)
  }

  # If got to the end and still no #%mend, throw error
  if (edx == length(lns)) {
    stop(paste0("Macro '", attr(ismacro, "name"), "' not closed. Did you forget a '#%mend'?"))
  }

  # Return start and end positions, which are used by calling function
  # to pull out the macro from the input lines so they are not re-scanned
  attr(ret, "start") <- bdx
  attr(ret, "end") <- edx

  return(ret)
}


# Whether or not a line is a macro call
# Has to be in globals macro list
#' @noRd
is_macro_call <- function(ln) {

  ret <- FALSE

  # If no begin with #%, skip
  dtct <- grepl("#%", ln, fixed = TRUE)[[1]]
  if (dtct) {

    # Remove the #%
    nl <- trimws(sub("#%", "", ln, fixed = TRUE)[[1]])

    # Find the (
    pos <- regexpr("(", nl, fixed = TRUE)[[1]]

    if (pos > 0) {

      # Get this macro name
      nm <- substring(nl, 1, pos - 1)

      # Get all macro names
      mnames <- names(gbl$macros)

      # See if this macro name is declared
      if (nm %in% mnames) {
        ret <- TRUE

        # Get parameter string
        prmstr <- substring(ln, pos)

        # Separate parameters
        prms <- get_parms(prmstr, nm, FALSE)

        # Return name and parameters as attributes
        attr(ret, "name") <- nm
        attr(ret, "parameters") <- prms
      }
    }
  }

  return(ret)

}

# Gets consolidated macro code for a macro call
# Accepts results of is_macro_call()
# mnm is macro name
# mfunc is the function definition
# cpm is the calling parameters from is_macro_call()
# ln is the line, in case we need to display for error handling
#' @noRd
get_macro_call <- function(mnm, mfunc, cpm, ln) {

  ret <- c()

  # Extract parameters and code
  prms <- mfunc$parameters
  cd <- mfunc$code

  # Get names from macro and call
  mnms <- names(prms)
  cnms <- names(cpm)

  if (length(mnms) > 0) {
    # Let macro drive the comparison
    for (idx in seq(1, length(mnms))) {

      # Get parameter name
      nm <- mnms[idx]

      # Default value
      dvl <- prms[[nm]]

      # Initialize value
      vl <- NULL

      # See if call uses it
      if (nm %in% cnms) {
         vl <- cpm[[nm]]
      } else {
        # NA means the parameter was not passed.
        # Not a problem if there is a default.
        if (!is.na(cnms[idx])) {

          # Blank means the parameter was not named.
          # Also not a problem.
          if (cnms[idx] == nm | cnms[idx] == "") {
            vl <- cpm[[idx]]
          }
        }
      }

      # If there is no default value and no called value, throw error.
      if (dvl == "" & is.null(vl)) {
        stop(paste0("Required parameter '", nm, "' for macro '", mnm, "' not found."))
      }

      # If there is a default value, use it
      if (is.null(vl) & dvl != "") {
        vl <- dvl
      }

      # Concat let statement
      ret[length(ret) + 1] <- paste0("#%let ", nm, " <- ", mreplace(vl))

    }
  }

  # Append code
  ret <- append(ret, cd)

  # Append macro end
  lln <- paste0("#%mend ", mnm)
  ret <- append(ret, lln)

  return(ret)
}

# Logging Utilities -------------------------------------------------------

# Writes a line of debug output
#' @noRd
log_debug <- function(vl, file_path = NULL, appnd = TRUE) {

  # See if there is a debug file or not
  if (is.null(file_path)) {
    pth <- gbl$debug_out
  } else {
    pth <- file_path
  }

  # Write to log or console
  cat(vl, "\n", file = pth, append = appnd)

}

# Captures error when debug is on
# Set up in msource()
#' @noRd
log_error <- function(msg = NULL) {

  # Get error message from system
  er <- geterrmessage()

  # Write to debug output
  log_debug(er)

  # Detach error handler
  options(error = NULL, warning.expression = NULL)

}

