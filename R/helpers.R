# Various utility functions

#' helper function to get ncdim4 object from nc by name
#'
#' @param nc a ncdf4 object as returned by \code{ncdf4::nc_open()}
#' @param dimname Name of ncdim4 object from \code{nc$dim}
#'
#' @export
getdim <- function(nc, dimname) {
  dimnames <- vapply(nc$dim, function(x) x$name, character(1))
  dimind <- match(dimname, dimnames, nomatch = NA)
  tryisna <- try(is.na(dimind))
  isna <- is.na(dimind)
  msg <- sprintf("dim %s not found", dimname)
  if (isna) stop(msg)
  out <- nc$dim[[dimind]]
  out
}

#' Helper function to recursively get names from an expression
getExprNames <- function(expr) {
  if (rlang::is_symbol(expr)) {
    return(rlang::as_string(expr))
  } else if (rlang::is_call(expr)) {
    return(lapply(expr[-1], getExprNames))
  }
}

#' Index an arbitrarily-sized array, from Hadley
#'
#' Copied from https://stackoverflow.com/a/14502298.
index_array <- function(x, dim, value, drop = FALSE) {
  # Create list representing arguments supplied to [
  # bquote() creates an object corresponding to a missing argument
  indices <- rep(list(bquote()), length(dim(x)))
  indices[[dim]] <- value

  # Generate the call to [
  call <- as.call(c(
    list(as.name("["), quote(x)),
    indices,
    list(drop = drop)))

  # Finally, evaluate it
  eval(call)
}

#' Helper function to subset a ncdim4 object
ssdim <- function(ncdim, keepinds) {
  ncdim$len <- length(keepinds)
  ncdim$vals <- ncdim$vals[keepinds]
  ncdim
}

