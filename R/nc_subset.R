#' Function to subset and write netcdf v4 files, including ones that have groups.
#'
#' Returns a list that can be used with \code{ncdf4::nc_create()}

nc_subset(pixcnc, pixel_cloud/points = pixel_cloud/latitude > 30)


nc_subset <- function(nc, dim, ...) {

  quo_named <- rlang::quos(...)
  if (any(nchar(names(quo_named)) < 1)) {
    stop("subexpressions must be in 'mutate' form, i.e.
           'lon = lon > 100' or 'lat = index > 10'")
  }
  quo_noname <- unname(quo_named)

  for (i in seq_along(quo_named)) {
    iname <- names(quo_named)[i]
    if (!iname %in% names(nc$dim)) {
      warning(sprintf("'%s' not found in netcdf variables, ignoring", iname))
      next
    }

    ivar <- quo_noname[[i]]
    if (!ivar %in% names(nc$var)) {
      warning(sprintf("'%s' not found in netcdf variables, ignoring", iname))
      next
    }

    idimnames <- purrr::map_chr(nc$var[[iname]], ~.$name)

    if (!identical(idimnames, iname)) {
      warning(sprintf("%s is not 1-D with dimension %s, ignoring", iname))
      next
    }







    SELECTION <- dplyr::filter(trans0[[iname]], !!!quo_noname[i])
    if (nrow(SELECTION) < 1L) {
      stop(sprintf("subexpression for [%s] results in empty slice",
                   iname))
    }

    trans0[[iname]]$selected <- trans0[[iname]]$index %in% SELECTION$index
  }

  dimlist <- nc$dim
  newdim <- dimlist[[dim]]
  if (is.null(newdim)) {
    stop("dim specified must be one of: ", paste(names(dimlist, collapse = ", ")))
  }

  # get indices corresponding to formula
  browser()
  qfmla <- enquo(formula)

  qexp <- quo_get_expr(qfmla)

  lhs <- qexp[[2]]



  # create list of ncvar4 objects with the subset variable
  # nc$dim

}
