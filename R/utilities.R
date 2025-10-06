


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
          assign(paste0(trimws(spl[1]), "."), vl, envir = e)

          # Clear out buffer if execution was successful
          if (!is.null(res)) {
            gbl$buffer <- c()
          }

        } else {

          # Assign value to macro variable
          vl <- trimws(mreplace(spl[2]))
          assign(paste0(trimws(spl[1]), "."), vl, envir = e)
        }

      } else {

        # Clear out macro variable
        assign(paste0(trimws(spl), "."), NULL, envir = e)
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

      tres <- eval(str2expression(mreplace(sysex)), envir = e)

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

      tres <- exists(paste0(sysex, "."), envir = e)
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

      vl <- eval(str2expression(trimws(nr)), envir = e)

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

    vl <- eval(str2expression(trimws(nr)), envir = e)

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

  dtct <- grepl("#%include", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    # Isolate path
    nl <- trimws(sub("#%include", "", ln, fixed = TRUE))
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

  dtct <- grepl("#%do", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    # Resolve any macro variables
    ln <- mreplace(ln)

    # Remove do token
    nl <- trimws(sub("#%do", "", ln, fixed = TRUE))

    # Split on equals sign
    snl <- strsplit(nl, "=", fixed = TRUE)[[1]]

    # Split on %to
    vr <- trimws(snl[1])
    cnl <- strsplit(snl[2], "%to", fixed = TRUE)[[1]]

    # Get starting number
    strt <- tryCatch({eval(str2expression(trimws(cnl[1])), envir = e)},
                     error = function(cond) {NA})

    # Get ending number
    end <- tryCatch({eval(str2expression(trimws(cnl[2])), envir = e)},
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

