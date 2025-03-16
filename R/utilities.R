


#' @noRd
is_let <- function(ln) {

  ret <- FALSE

  dtct <- grepl("#%let", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    nl <- trimws(sub("#%let", "", ln, fixed = TRUE))

    spl <- strsplit(nl, "=", fixed = TRUE)[[1]]

    if (length(spl) > 1) {

      vl <- eval(str2expression(trimws(spl[2])), envir = e)
      assign(paste0(trimws(spl[1]), "."), vl, envir = e)

    } else {

      assign(paste0(trimws(spl), "."), NULL, envir = e)
    }
  }


  return(ret)
}

#' @noRd
is_if <- function(ln) {

  ret <- FALSE

  dtct <- grepl("#%if", ln, fixed = TRUE)[[1]]
  if (dtct) {
    ret <- TRUE

    nl <- trimws(sub("#%if", "", ln, fixed = TRUE))

    vl <- eval(str2expression(trimws(nl)), envir = e)

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

    vl <- eval(str2expression(trimws(nl)), envir = e)

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


