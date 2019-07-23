context("netcdf subsetting")

# test_that("subsetted variables have correct structure", {
#   foonc <- ncdf4::nc_open("rt.nc")
#   on.exit(ncdf4::nc_close(foonc))
#   fooinds <- ncss_indlist(foonc,
#                           `nodes/node_id` < median(`nodes/node_id`,
#                                                    na.rm = TRUE))
#   foodim <- ncss_dimlist(foonc, fooinds)
#   foovar <- ncss_varlist(foonc, foodim)
#
#   foovarnames <- vapply(foovar, function(x) x$name, character(1))
#   foodimnames <- vapply(foodim, function(x) x$name, character(1))
#
#   for(i in seq_along(foovar)) {
#     tfilei <- tempfile()
#     nci <- ncdf4::nc_create(tfilei, vars = foovar[[i]])
#     ncdf4::nc_close(nci)
#     unlink(tfilei)
#   }
#
#   ncdf4::nc_close(foonc)
# })

test_that("subsetting creates a ncdf4 object", {
  foofile <- tempfile()
  testfile <- "rt.nc"
  foonc <- ncdf4::nc_open(testfile)
  ssnc <- nc_subset(foonc,
                    `nodes/node_id` < median(`nodes/node_id`, na.rm = TRUE),
                    filename = foofile)
  ncdf4::nc_close(foonc)
  expect_is(ssnc, "ncdf4")
  expect_lt(file.size(foofile), file.size(testfile))
})


test_that("optimizing ncvar_get call returns same as non-optimized", {
  testfile <- "rt.nc"
  foonc <- ncdf4::nc_open(testfile)

  ssinds1 <- ncss_indlist(foonc,
                     `nodes/node_id` < median(`nodes/node_id`,
                                              na.rm = TRUE),
                     `reaches/reaches` == 1)
  ssvals1 <- lapply(names(foonc$var), ncvar_getss,
                    nc = foonc,
                    indlist = ssinds1, optimize = FALSE)
  ssvals2 <- lapply(names(foonc$var), ncvar_getss,
                    nc = foonc,
                    indlist = ssinds1, optimize = TRUE)

  expect_identical(ssvals1, ssvals2)
})
