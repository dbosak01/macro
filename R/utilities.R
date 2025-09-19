


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
      vl <- trimws(mreplace(spl[2]))
      assign(paste0(trimws(spl[1]), "."), vl, envir = e)

    } else {

      assign(paste0(trimws(spl), "."), NULL, envir = e)
    }
  }

  return(ret)
}

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

      tres <- eval(str2expression(mreplace(sysex)), envir = e)
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

get_include <- function(pth) {


  if (!file.exists(pth)) {

    stop(paste0("Include file '", pth, "' not found."))

  }

  f <- file(pth, encoding = "UTF-8")

  ret <- readLines(f, warn = FALSE)

  close(f)

  return(ret)

}



is_do <- function(ln) {

  ret <- FALSE

  dtct <- grepl("#%do", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    nl <- trimws(sub("#%do", "", ln, fixed = TRUE))
    snl <- strsplit(nl, "=", fixed = TRUE)[[1]]

    vr <- trimws(snl[1])
    cnl <- strsplit(snl[2], "%to", fixed = TRUE)[[1]]

    strt <- tryCatch({eval(str2expression(trimws(cnl[1])), envir = e)},
                     error = function(cond) {NA})

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

do_info <- function(lvl, dolvl, var, start, end) {

  di <- structure(list(), class = c("do_info", "list"))


  di$lvl <- lvl
  di$dolvl <- dolvl
  di$var <- var
  di$start <- start
  di$end <- end
  di$code <- NULL
  di$cnt <- NULL

  return(di)

}





