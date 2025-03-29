


#' @noRd
is_let <- function(ln) {

  ret <- FALSE

  dtct <- grepl("#%let", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    nl <- trimws(sub("#%let", "", ln, fixed = TRUE))

    spl <- strsplit(nl, "=", fixed = TRUE)[[1]]

    if (length(spl) > 1) {

      # vl <- eval(str2expression(trimws(spl[2])), envir = e)
      # vl <- str2expression(trimws(spl[2]))
      vl <- trimws(spl[2])
      assign(paste0(trimws(spl[1]), "."), vl, envir = e)

    } else {

      assign(paste0(trimws(spl), "."), NULL, envir = e)
    }
  }

  return(ret)
}

sub_sysfunc <- function(ln) {

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
    for (chr in splt) {
      if (chr == "(") {
        open <- open + 1
      }
      if (chr == ")") {
        open <- open - 1
      }
      if (open == 0) {
        epos <- idx
        sysex <- substring(ln, spos + 1, epos - 1)
        break
      }
      idx <- idx + 1
    }
    if (sysex != "") {

      tres <- eval(str2expression(sysex), envir = e)
      ret <- paste0(substring(ln, 1, pos -1),
                   as.character(tres),
                   substring(ln, epos + 1))
    }
  }

  return(ret)
}


#' @noRd
is_if <- function(ln) {

  ret <- FALSE
  # browser()
  dtct <- grepl("#%if", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    nl <- trimws(sub("#%if", "", ln, fixed = TRUE))

    nr <- mreplace(nl)

    vl <- eval(str2expression(trimws(nr)), envir = e)

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


