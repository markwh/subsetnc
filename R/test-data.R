# Preparing data for tests and extdata.

# Start with full-size netcdfs.
library(ncdf4)
library(purrr)

# pixel cloud, pixcvec
# TODO: replace with url
raw_pixcfile <- "D:/data/riverobs-output/sacruns_20190709/90/pixel_cloud.nc"
raw_pcvfile <- "D:/data/riverobs-output/sacruns_20190709/90/pcv.nc"
raw_rtfile <- "D:/data/riverobs-output/sacruns_20190709/90/rt.nc"

test_pixcfile <- "tests/testthat/pixel_cloud.nc"
test_pcvfile <- "tests/testthat/pcv.nc"
test_rtfile <- "tests/testthat/rt.nc"

ext_pixcfile <- "extdata/pixel_cloud.nc"
ext_pcvfile <- "extdata/pcv.nc"
ext_rtfile <- "extdata/rt.nc"

# rivertile file is small enough not to have to subset. TODO: make this smaller
file.copy(raw_rtfile, test_rtfile)

pixcnc <- nc_open(raw_pixcfile)
pcvnc <- nc_open(raw_pcvfile)

# Get subset indices, making sure they match pcv to pixc.
pixcrange <- ncvar_get(pixcnc, "pixel_cloud/range_index")
pixcazim <- ncvar_get(pixcnc, "pixel_cloud/azimuth_index")
pcvrange <- ncvar_get(pcvnc, "range_index")
pcvazim <- ncvar_get(pcvnc, "azimuth_index")
pcvnode <- ncvar_get(pcvnc, "node_index")
pixcra <- pixcrange * 1e5 + pixcazim
pcvra <- pcvrange *1e5 + pcvazim

ramatches <- match(pcvra, pixcra)

# subset indices, based on matching pixc to pcv
selnodes <- sort(unique(pcvnode))[3:4]
keepinds_pcv <- which(pcvnode %in% selnodes)
keepinds_pixc <- ramatches[keepinds_pcv]

n_keep_ex <- length(keepinds_pcv)
n_keep_test <- 100

# Names of ncdf variables to modify
pixcvars <- names(pixcnc$var)[grepl("^pixel_cloud/", names(pixcnc$var))]
tvpvars <- names(pixcnc$var)[grepl("^tvp/", names(pixcnc$var))]
noisevars <- names(pixcnc$var)[grepl("^noise/", names(pixcnc$var))]

# redefine variables
# first, redefine dimensions
inddimname <- "pixel_cloud/points"
newpixcdim <- pixcnc$dim[[inddimname]]
newpixcdim$len <- n_keep
newpixcdim$vals <- 1:n_keep

tvpdimname <- "tvp/num_tvps"
newtvpdim <- pixcnc$dim[[tvpdimname]]
newtvpdim$len <- n_keep
newtvpdim$vals <- 1:n_keep

noisedimname <- "noise/num_lines"
newnoisedim <- pixcnc$dim[[noisedimname]]
newnoisedim$len <- n_keep
newnoisedim$vals <- 1:n_keep

# Function to subset a variable in a pixel cloud, returns ncvar4 object
ncvar_sset <- function(pixcnc, name, inds, newdim) {

  var1 <- pixcnc$var[[name]]

  if (var1$ndims > 1) {
    if (var1$ndims > 2)
      stop(sprintf("Variable %s has more than 2 dimensions.", var1$name))
    newsize <- c(var1$size[1], length(inds))
  } else {
    newsize <- length(inds)
  }

  # redefine dim
  dimid <- which(map_chr(var1$dim, ~.$name) ==  newdim$name)
  var1$dim[[dimid]] <- newdim

  var1$size <- var1$varsize <- newsize
  var1$chunksizes <- NA
  var1
}

# create list of variables (ncvar4)
varlist_sset <- map(pixcvars,
                    ~ncvar_sset(pixcnc, ., keepinds_pixc, newdim = newpixcdim))
tvplist_sset <- map(tvpvars,
                    ~ncvar_sset(pixcnc, ., keepinds_pixc, newdim = newtvpdim))
noiselist_sset <- map(noisevars,
                      ~ncvar_sset(pixcnc, ., keepinds_pixc, newdim = newnoisedim))
varlist_full <- map(pixcnc$var, function(x) {x$chunksizes <- NA; x})
varlist_full[pixcvars] <- varlist_sset
varlist_full[tvpvars] <- tvplist_sset
varlist_full[noisevars] <- noiselist_sset

# create netcdf with the new variables
newnc <- nc_create(test_pixcfile, vars = varlist_full)

# subset values, put into a list, then nc_put into new nc.
ssetvar <- function(array, sset) {
  out <- try(if (length(dim(array)) > 1) array[, sset] else array[sset])
  # print(out)
  if (inherits(out, "try-error")) browser()
  out
}

newvals <- map(names(pixcnc$var), ~ncvar_get(pixcnc, .)) %>%
  map(ssetvar, keepinds_pixc) %>%
  setNames(pixcvars)

map2(names(pixcnc$var), newvals, ~ncvar_put(newnc, .x, vals = .y))

nc_close(pixcnc)
nc_close(newnc)


# pixcvec subset ----------------------------------------------------------

pcvdimname <- "points"
newpcvdim <- pcvnc$dim[[pcvdimname]]
newpcvdim$len <- n_keep
newpcvdim$vals <- 1:n_keep

newvals_pcv <- map(names(pcvnc$var), ~ncvar_get(pcvnc, .)) %>%
  map(ssetvar, keepinds_pcv) %>%
  setNames(names(pcvnc$var))

# R's netcdf can't handle unsigned bytes, so need to change some missing values.
pcvvars <- map(names(pcvnc$var),
               ~ncvar_sset(pcvnc, ., keepinds_pcv, newdim = newpcvdim)) %>%
  setNames(names(pcvnc$var)) %>%
  map(function(x) {x$prec <- ifelse(x$prec == "unsigned byte", "byte", x$prec); x}) %>%
  map(function(x) {x$missval <- ifelse(x$missval == 255L, 127L, x$missval); x})


newnc_pcv <- nc_create(test_pcvfile, pcvvars)

map2(names(pcvnc$var), newvals_pcv, ~ncvar_put(newnc_pcv, .x, vals = .y))

nc_close(pcvnc)
nc_close(newnc_pcv)



# Prior reach database ----------------------------------------------------

cl1 <- priorcl_read("~/Documents/swot-error/data/priordb-update/Sac_sample_db15.nc")
