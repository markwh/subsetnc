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

test_that("subsetting variables creates a ncdf4 object", {
  foofile <- tempfile()
  testfile <- "rt.nc"
  foonc <- ncdf4::nc_open(testfile)
  ssnc <- nc_subset(foonc,
                    `nodes/node_id` < median(`nodes/node_id`, na.rm = TRUE),
                    filename = foofile)
  ncdf4::nc_close(foonc)
  expect_is(ssnc, "ncdf4")

  expect_gt(length(ssnc$var), length(foonc$var))
  expect_false(is.null(ssnc$var[["nodes/nodes__"]]))

  expect_lt(file.size(foofile), file.size(testfile))
})


test_that("subsetting dimensions creates a ncdf4 object", {
  foofile <- tempfile()
  testfile <- "rt.nc"
  foonc <- ncdf4::nc_open(testfile)
  ssnc <- nc_subset(foonc,
                    `nodes/nodes` < median(`nodes/nodes`, na.rm = TRUE),
                    filename = foofile)
  ncdf4::nc_close(foonc)
  expect_is(ssnc, "ncdf4")

  expect_gt(length(ssnc$var), length(foonc$var))
  expect_false(is.null(ssnc$var[["nodes/nodes__"]]))

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


test_that("problematic prec types are reassigned with message", {
  dim1 <- ncdf4::ncdim_def("dim1", units = "", vals = 1:20)
  badvar1 <- ncdf4::ncvar_def("var1", units = "m", dim = dim1, prec = "byte")
  badvar1$prec <- "unsigned byte"
  badvar2 <- ncdf4::ncvar_def("var1", units = "m", dim = dim1, prec = "double")
  badvar2$prec <- "8 byte int"

  badvarlist <- list(var1 = badvar1, var2 = badvar2)

  expect_message(goodvarlist <- reassign_prec(badvarlist))
  expect_true(goodvarlist[[1]]$prec == "byte")
  expect_true(goodvarlist[[2]]$prec == "double")


})
