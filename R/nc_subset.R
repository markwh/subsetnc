#' Function to subset and write netcdf v4 files, including ones that have groups.
#'
#' Returns a list that can be used with \code{ncdf4::nc_create()}


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
  # Print it, just to make it easier to see what's going on
  # print(call)

  # Finally, evaluate it
  eval(call)
}


#' Return vector of indices (not dimension values)
#'
#' @param nc a ncdf4 object as returned by `ncdf4::nc_open()`
#' @param ... Logical expressions involving variables in nc
#'
#' @importFrom ncdf4 ncvar_get
#' @export
ncss_indlist <- function(nc, ...) {
  ssexprs <- rlang::enexprs(...)

  outlist <- list()

  for (i in 1:length(ssexprs)) {

    ssexpri <- ssexprs[[i]]

    namesi <- unlist(getExprNames(ssexprs[[i]]))
    namesi <- intersect(namesi, names(nc$var))
    dimsi <- lapply(namesi, function(x) nc$var[[x]]$dim)

    dimlensi <- vapply(dimsi, length, integer(1))
    if (max(dimlensi) > 1) stop("All subsetting variables must be 1-dimensional")

    dimnamesi <- vapply(dimsi, function(x) x[[1]][["name"]], character(1))
    if (length(unique(dimnamesi)) > 1)
      stop("All subsetting variables must have same dimension.")


    # build data mask
    dfi_list <- lapply(namesi, function(x) as.vector(ncvar_get(nc, x)))
    dfi <- setNames(as.data.frame(dfi_list), namesi)

    indsi <- which(rlang::eval_tidy(ssexpri, dfi))


    outlist[[dimnamesi]] <- if (is.null(outlist[[dimnamesi]])) indsi else {
      intersect(indsi, outlist[[dimnamesi]])
    }

  }

  outlist

}

#' Create a list of ncdimr4 objects from a subset list
#'
#' @param nc ncdf4 object, as returned by \code{ncdf4::nc_open()}
#' @param indlist As returne by \code{ncss_indlist()}
#'
#' @export
ncss_dimlist <- function(nc, indlist) {
  out <- nc$dim

  for (i in 1:length(indlist)) {
    namei <- names(indlist)[i]

    out[[namei]] <- ssdim(out[[namei]], indlist[[i]])
  }
  out
}

#' Helper function to subset a ncdim4 object
ssdim <- function(ncdim, keepinds) {
  ncdim$len <- length(keepinds)
  ncdim$vals <- ncdim$vals[keepinds]
  ncdim
}

#' Create a list of ncvar4 objects from a subset list
#'
#' @param nc ncdf4 object, as returned by \code{ncdf4::nc_open()}
#' @param dimlist As returne by \code{ncss_dimlist()}
#'
#' @export
ncss_varlist <- function(nc, dimlist) {
  varlist <- nc$var
  outlist <- varlist
  for (i in 1:length(outlist)) {
    vari <- varlist[[i]]
    vari$size <- dimsize(vari, dimlist)
    vari$chunksizes <- NA # Unclear why I need to do this.

    dimnamesi <- vapply(vari$dim, function(x) x$name, character(1))
    vari$dim <- dimlist[dimnamesi]
    vari$varsize <- vari$size # unclear why nc object contains redundant info

    outlist[[vari$name]] <- vari
  }

  outlist
}

#' Helper function to get dimension sizes of a ncvar4 object.
#'
#' @param var ncvar4 object
#' @param dimlist as returned by \code{ncss_dimlist()}
#'
dimsize <- function(var, dimlist) {
  dimnames <- vapply(var$dim, function(x) x$name, character(1))
  out <- vapply(dimlist[dimnames], function(x) x$len, numeric(1))
  unname(out)
}

#' Get a subset of a single netcdf variable
#'
#' @param indlist As returne by \code{ncss_indlist()}
#' @inheritParams ncdf4::ncvar_get
#'
#' @export
ncvar_getss <- function(nc, varid, indlist = NULL, verbose = FALSE,
                        signedbyte = TRUE, collapse_degen = TRUE,
                        raw_datavals = FALSE) {

  stopifnot(is.character(varid))

  ndims <- length(nc$dim)

  vari <- nc$var[[varid]]
  dimsi <- vari$dim
  dimnamesi <- vapply(dimsi, function(x) x$name, character(1))

  out <-  ncvar_get(nc, varid, start = NA,
                    count = NA,
                    verbose = verbose, signedbyte = signedbyte,
                    collapse_degen = collapse_degen,
                    raw_datavals = raw_datavals)

  # Do subsetting in order
  ssdimnos <- match(names(indlist), dimnamesi)
  if (is.na(ssdimnos)) return(out) # no matches

  for (i in seq_along(ssdimnos)) {
    dimno <- ssdimnos[i]
    out <- index_array(out, dimno, indlist[[i]])
  }

  out
}

#' Subset a netcdf
#'
#' Creates (and writes) a subset of a ncdf4 object using dplyr-like expressions
#'
#'
#' @param nc a ncdf4 object, as returned by \code{ncdf4::nc_open()}
#' @param ... \code{dplyr::filter}-like expressions. See details.
#' @param filename Where to write the new netcdf? Defaults to \code{tempfile()}.
#' @param keep_open Keep the netcdf open? Defaults to TRUE.
#'
#' @export
nc_subset <- function(nc, ..., filename = tempfile(), keep_open = TRUE) {

  indlist <- ncss_indlist(nc, ...)
  dimlist <- ncss_dimlist(nc, indlist)
  varlist <- ncss_varlist(nc, dimlist)

  ncss_create_fill(nc, filename = filename, varlist = varlist,
                   indlist = indlist, keep_open = keep_open)

}

#' Create and fill a new netcdf as a subset of an existing netcdf
#'
#' Used internally by \code{nc_subset()}
#'
#' @inheritParams nc_subset
#' @param varlist as returned by \code{ncss_varlist()}
#' @param indlist as returned by \code{ncss_indlist()}
#'
#' @importFrom ncdf4 nc_open nc_close
ncss_create_fill <- function(nc, filename, varlist, indlist, keep_open = TRUE) {
  newnc <- ncdf4::nc_create(filename, vars = varlist)
  if (!keep_open) on.exit(nc_close(newnc))
  for (var in nc$var) {
    varnamei <- var$name
    valsi <- ncvar_getss(nc, var$name, indlist = indlist)
    valsi <- val_check(var, valsi)
    ncdf4::ncvar_put(newnc, varlist[[varnamei]], vals = valsi)
  }
  # Close and reopen to take out of write mode.
  nc_close(newnc)
  newnc <- nc_open(filename)
  invisible(newnc)
}

val_check <- function(ncvar, vals) {
  # Make sure numeric values are finite, else replace with missing.
  if (ncvar$prec != "char") {
    missval <- ncvar$missval
    if (is.null(missval)) {
      warning("imposing a missing value of NA on non-finite values.")
      missval <- NA
    }
    vals[!is.finite(vals)] <- ncvar$missval
  }
  vals
}

