

# Detection Utilities -----------------------------------------------------



is_let <- function(ln, opn = TRUE) {

  ret <- FALSE

  dtct <- grepl("#%let", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    if (opn) {
      # Remove %let keyword
      nl <- trimws(sub("#%let", "", ln, fixed = TRUE))

      spl <- c()
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
          vl <- eval(str2expression(nvl), envir = lcl)
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


#' @noRd
#' @import fmtr
sub_funcs <- function(ln) {

  # browser()

  ret <- ln

  pos <- regexpr("%sysfunc(", ln, fixed = TRUE)[[1]]
  if (pos > 0) {

    spos <- pos + 8
    tmp <- substring(ln, spos)
    epos <- nchar(ln)
    splt <- strsplit(tmp, "", fixed = TRUE)[[1]]
    open <- 0
    sysex <- ""
    idx <- spos
    cma <- NA
    fmt <- NA
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

      tres <- eval(str2expression(mreplace(sysex)), envir = gbl$env)

      if (!is.na(fmt)) {

         tres <- fapply(tres, fmt)
      }
      ret <- paste0(substring(ln, 1, pos -1),
                    as.character(tres),
                    substring(ln, epos + 1))
    }
  }

  pos2 <- regexpr("%symexist(", ret, fixed = TRUE)[[1]]
  if (pos2 > 0) {

    spos <- pos2 + 9
    tmp <- substring(ret, spos)
    epos <- nchar(ret)
    splt <- strsplit(tmp, "", fixed = TRUE)[[1]]
    open <- 0
    sysex <- ""
    idx <- spos
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
      # tres <- exists(paste0(sysex, "."), envir = gbl$env)
      tres <- exists(paste0("&", sysex), envir = gbl$env)
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
  dtct <- grepl("#%if", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    if (evaluate) {
      nl <- trimws(sub("#%if", "", ln, fixed = TRUE))

      nr <- mreplace(nl)

      vl <- eval(str2expression(trimws(nr)), envir = gbl$env)

    } else {

      vl <- NA
    }

    attr(ret, "value") <- vl
  }

  return(ret)
}


#' @noRd
is_end <- function(ln) {

  ret <- FALSE

  dtct <- grepl("#%end", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE
  }

  return(ret)
}

#' @noRd
is_elseif <- function(ln) {

  ret <- FALSE

  dtct <- grepl("#%elseif", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    nl <- trimws(sub("#%elseif", "", ln, fixed = TRUE))

    nr <- mreplace(nl)

    vl <- eval(str2expression(trimws(nr)), envir = gbl$env)

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

  dtct <- grepl("#%", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE
  }

  return(ret)
}


#' @noRd
is_include <- function(ln) {

  ret <- FALSE

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

    if (is.na(strt) | is.na(end)) {
      stop("Macro do loop improperly formed.")

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

#' @noRd
sub_mvar <- function(ln, varnm, varvl) {

  rvl <- varnm
  pos <- regexpr(rvl, ln, fixed = TRUE)
  epos <- pos + nchar(rvl) - 1
  ftick <- FALSE

  if (pos > 1) {
    if (substring(ln, pos - 1, pos - 1) == "`") {
      ftick <- TRUE
    }
  }

  if (epos < nchar(ln)) {
    if (substring(ln, epos, epos) != ".") {
      if (substring(ln, epos + 1, epos + 1) == ".") {
        rvl <- paste0(rvl, ".")
      }
    }

    if (substring(ln, epos + 1, epos + 1) == "`") {
      if (ftick) {
        rvl <- paste0("`", rvl, "`")
      }
    }
  }

  if (epos + 1 < nchar(ln)) {
    if (substring(ln, epos + 2, epos + 2) == "`") {
      if (ftick) {
        rvl <- paste0("`", rvl, "`")
      }
    }
  }

  ret <- sub(rvl, varvl, ln, fixed = TRUE)

  return(ret)

}

# Macro Utilities ---------------------------------------------------------


# Beginning of macro definition
#' @noRd
is_macro <- function(ln) {

  ret <- FALSE

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
  ret <- list()
  lvl <- 1
  pnm <- c()
  vl <- c()
  nmflg <- TRUE

  for (ch in sprms) {
    if (lvl == 1) {
      if (ch == "(") {
        lvl <- lvl + 1

        if (nmflg) {
          pnm <- append(pnm, ch)
        } else {
          vl <- append(vl, ch)
        }

      }  else if (ch == "=") {

        nmflg <- FALSE
        pnm <- trimws(paste0(pnm, collapse = ""))

      } else if (ch == ",") {

        nmflg <- TRUE
        pnm <- trimws(paste0(pnm, collapse = ""))
        ret[[pnm]] <- trimws(paste0(vl, collapse = ""))
        pnm <- c()
        vl <- c()

      } else {
        if (nmflg) {
          pnm <- append(pnm, ch)
        } else {
          vl <- append(vl, ch)
        }
      }
    } else {
      if (ch == "(") {
        lvl <- lvl + 1
      } else if (ch == ")") {
        lvl <- lvl - 1
      }

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
  lvl <- 1
  bdx <- idx + 1
  edx <- length(lns)

  for (sdx in seq(bdx, length(lns))) {

    ln <- lns[sdx]

    ismacro <- is_macro(ln)

    if (as.logical(ismacro)) {
      lvl <- lvl + 1
    } else if (as.logical(is_mend(ln))) {
      lvl <- lvl - 1

      if (lvl == 0) {
        edx <- sdx - 1
        break
      }
    }
    ret <- append(ret, ln)
  }

  if (edx == length(lns)) {
    stop(paste0("Macro '", attr(ismacro, "name"), "' not closed. Did you forget a '#%mend'?"))
  }

  attr(ret, "start") <- bdx
  attr(ret, "end") <- edx

  return(ret)
}


# Whether or not a line is a macro call
# Has to be in globals macro list
#' @noRd
is_macro_call <- function(ln) {

  ret <- FALSE

  dtct <- grepl("#%", ln, fixed = TRUE)[[1]]
  if (dtct) {

    nl <- trimws(sub("#%", "", ln, fixed = TRUE)[[1]])
    pos <- regexpr("(", nl, fixed = TRUE)[[1]]

    if (pos > 0) {

      # Get this macro name
      nm <- substring(nl, 1, pos - 1)

      # Get all macro names
      mnames <- names(gbl$macros)

      # See if this macro name is declared
      if (nm %in% mnames) {
        ret <- TRUE

        prmstr <- substring(ln, pos)

        prms <- get_parms(prmstr, nm, FALSE)

        attr(ret, "name") <- nm
        attr(ret, "parameters") <- prms
      }
    }
  }

  return(ret)

}

# Gets consolidated macro code for a macro call
# Accepts results of is_macro_call()
#' @noRd
get_macro_call <- function(mnm, mfunc, cpm) {

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
        if (cnms[idx] == nm | cnms[idx] == "") {
          vl <- cpm[[idx]]
        }
      }

      if (dvl == "" & is.null(vl)) {
        stop(paste0("Required parameter '", nm, "' for macro '", mnm, "' not found."))
      }

      if (is.null(vl) & dvl != "") {
        vl <- dvl
      }

      # Concat let statement
      ret[length(ret) + 1] <- paste0("#%let ", nm, " <- ", vl)

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

#' @noRd
log_debug <- function(vl, file_path = NULL, appnd = TRUE) {

  if (is.null(file_path)) {
    pth <- gbl$debug_out
  } else {
    pth <- file_path
  }

  # Write to log or console
  cat(vl, "\n", file = pth, append = appnd)

}

#' @noRd
log_error <- function(msg = NULL) {

  # Get error message from system
  er <- geterrmessage()

  # Write to debug output
  log_debug(er)

  # Detach error handler
  options(error = NULL, warning.expression = NULL)

}

