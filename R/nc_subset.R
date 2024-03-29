#' Function to subset and write netcdf v4 files, including ones that have groups.
#'
#' Returns a list that can be used with \code{ncdf4::nc_create()}


#' Return vector of indices (not dimension values)
#'
#' @param nc a ncdf4 object as returned by \code{ncdf4::nc_open()}
#' @param ... Logical expressions involving variables in nc
#' @param env passed to \code{rlang::eval_tidy()}
#'
#' @importFrom ncdf4 ncvar_get
#' @importFrom stats setNames
#' @export
ncss_indlist <- function(nc, ..., env = parent.frame()) {
  ssexprs <- rlang::enexprs(...)

  outlist <- list()

  for (i in 1:length(ssexprs)) {
    # Expression may contain var names and dim names. Need to
    # potentially put both into data mask.
    ssexpri <- ssexprs[[i]]

    # all names (symbols) from current expression
    names0 <- unlist(getExprNames(ssexprs[[i]]))
    varnamesi <- intersect(names0, names(nc$var)) # just var names
    dimnamesi <- intersect(names0, names(nc$dim)) # just dim names

    # ncdim4, dimnames corresponding to var names
    vardimsi <- lapply(varnamesi, function(x) nc$var[[x]]$dim)
    vardimnamesi <- vapply(vardimsi, function(x) x[[1]]$name, character(1))

    # Checks on comparability between supplied variables, dims
    dimlensi <- vapply(vardimsi, length, integer(1))
    if (length(dimlensi) && (max(dimlensi) > 1))
      stop("All subsetting variables must be 1-dimensional")
    alldimnamesi <- unique(c(vardimnamesi, dimnamesi))
    if (length(alldimnamesi) != 1)
      stop("All subsetting variables must have same dimension.",
           ssexpri)


    # build data mask
    dfi_varlist <- lapply(varnamesi, function(x) as.vector(ncvar_get(nc, x)))
    dfi_dimlist <- list(getdim(nc, alldimnamesi)$vals)
    dfi <- setNames(as.data.frame(c(dfi_varlist, dfi_dimlist)),
                    c(varnamesi, alldimnamesi))

    indsi <- which(rlang::eval_tidy(ssexpri, dfi, env = env))

    outlist[[alldimnamesi]] <- if (is.null(outlist[[alldimnamesi]])) indsi else {
      intersect(indsi, outlist[[alldimnamesi]])
    }

    stopifnot(inherits(outlist[[alldimnamesi]], "integer"))

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
    # print("units: ")
    # print(out[[namei]]$units)
    if (out[[namei]]$units == "") { # Requires dim values to be 1:length(vals)
      out[[namei]]$vals <- seq_along(out[[namei]]$vals)
    }
  }
  out
}



#' Create a list of ncvar4 objects from a subset list
#'
#' @param nc ncdf4 object, as returned by \code{ncdf4::nc_open()}
#' @param dimlist As returne by \code{ncss_dimlist()}
#' @param reassign_prec Force data type into ones \code{ncdf4} can handle?
#'   Currently required to work with ncdf4.
#'
#' @export
ncss_varlist <- function(nc, dimlist, reassign_prec = TRUE) {
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

  # Create new variables for original dimension values
  for (i in seq_along(dimlist)) {
    dimnamei <- dimlist[[i]]$name
    varnamei <- paste0(dimnamei, "__")
    newvari <- ncdf4::ncvar_def(varnamei, units = "1",
                                dim = dimlist[[i]], prec = "integer")
    outlist[[varnamei]] <- newvari
  }

  if (reassign_prec) {
    outlist <- reassign_prec(outlist)
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
#' Note: \code{collapse_degen} (from \code{ncdf4::ncvar_get}
#'  is always FALSE here, in order to
#'  allow subsetting of resulting arrays.
#'
#' @param indlist As returned by \code{ncss_indlist()}
#' @param optimize optimize call to \code{ncdf4::ncvar_get}
#' @inheritParams ncdf4::ncvar_get
#'
#' @export
ncvar_getss <- function(nc, varid, indlist = NULL, verbose = FALSE,
                        signedbyte = TRUE,
                        raw_datavals = FALSE, optimize = TRUE) {

  stopifnot(is.character(varid))

  ndims <- length(nc$dim)

  vari <- nc$var[[varid]]
  dimsi <- vari$dim
  dimnamesi <- vapply(dimsi, function(x) x$name, character(1))
  names(dimsi) <- dimnamesi

  # Make start and count arguments, adjust indlist accordingly
  commondims <- intersect(dimnamesi, names(indlist)) # only special if in both inslist and var
  indlistinds <- match(commondims, names(indlist))
  vardiminds <- match(commondims, dimnamesi)

  starts <- rep(1L, length(dimnamesi))
  counts <- rep(-1L, length(dimnamesi))

  if (optimize) {
    starts[vardiminds] <- vapply(indlist[indlistinds], min,
                                  integer(1), na.rm = TRUE)
    counts[vardiminds] <- vapply(indlist[indlistinds],
                              function(x, ...) max(x, ...) - min(x, ...) + 1L,
                              integer(1), na.rm = TRUE)
    indlist[indlistinds] <- purrr::map2(indlist[indlistinds],
                                        starts[vardiminds],
                                        function(x, y) x - y + 1L)

  }

  out <-  ncvar_get(nc, varid, start = starts, count = counts,
                    verbose = verbose, signedbyte = signedbyte,
                    collapse_degen = FALSE,
                    raw_datavals = raw_datavals)

  # Do subsetting in order--only commondims
  for (i in seq_along(vardiminds)) {
    vardimno <- vardiminds[i]
    inddimno <- indlistinds[i]
    out <- index_array(out, vardimno, indlist[[inddimno]])
  }

  out
}

#' Subset a netcdf
#'
#' Creates (and writes) a subset of a ncdf4 object using dplyr-like expressions
#'
#' The new netcdf is a subset of the original, and also includes new 1-D
#'  variables containing the original dimensions' values.
#'
#' @param nc a ncdf4 object, as returned by \code{ncdf4::nc_open()}
#' @param ... \code{dplyr::filter}-like expressions. See details.
#' @param filename Where to write the new netcdf? Defaults to \code{tempfile()}.
#' @param keep_open Keep the netcdf open? Defaults to TRUE.
#'
#' @export
nc_subset <- function(nc, ..., filename = tempfile(), keep_open = TRUE) {

  indlist <- ncss_indlist(nc, ..., env = parent.frame())
  dimlist <- ncss_dimlist(nc, indlist)
  varlist <- ncss_varlist(nc, dimlist)

  ncss_create_fill(nc, filename = filename, varlist = varlist,
                   indlist = indlist, keep_open = keep_open, optimize = TRUE)

}

#' Create and fill a new netcdf as a subset of an existing netcdf
#'
#' Used internally by \code{nc_subset()}
#'
#' The new netcdf is a subset of the original, and also includes new 1-D
#'  variables containing the original dimensions' values.
#'
#' @inheritParams nc_subset
#' @param varlist as returned by \code{ncss_varlist()}
#' @param indlist as returned by \code{ncss_indlist()}
#' @param optimize use \code{start} and \code{count} arguments to optimize
#'   \code{ncvar_get()} call?
#'
#' @importFrom ncdf4 nc_open nc_close
ncss_create_fill <- function(nc, filename, varlist, indlist, keep_open = TRUE,
                             optimize = TRUE) {

  newnc <- ncdf4::nc_create(filename, vars = varlist)
  if (!keep_open) on.exit(nc_close(newnc))

  # Fill with subset variables' values
  for (var in nc$var) {
    varnamei <- var$name
    valsi <- ncvar_getss(nc, var$name, indlist = indlist, optimize = optimize)
    valsi <- val_check(var, valsi)

    counts <- vapply(varlist[[varnamei]]$dim, function(x) x[["len"]], numeric(1))

    if (is.null(counts)) browser()
    ncdf4::ncvar_put(newnc, varlist[[varnamei]], vals = valsi,
                     start = rep(1L, length(counts)),
                     count = counts)
  }

  # Fill new variables for original dimension values
  for (i in seq_along(indlist)) {
    dimnamei <- names(indlist)[i]
    varnamei <- paste0(dimnamei, "__")
    ncdf4::ncvar_put(newnc, varlist[[varnamei]], vals = indlist[[i]],
                     start = 1L, count = length(indlist[[i]]))
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
    vals[!is.finite(vals)] <- missval
  }
  vals
}

