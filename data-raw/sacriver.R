## code to prepare `sacriver` dataset goes here

usethis::use_data("sacriver")

origncfile <- "~/Documents/swot-error/output/sac/47/rt.nc"
orignc <- nc_open(origncfile)

orignc$var$nodesrea

newnc <- nc_subset(orignc,
                   `reaches/reaches` == `reaches/reaches`[2],
                   `nodes/reach_id` == `nodes/reach_id`[2],
                   filename = "inst/extdata/sacriver.nc", keep_open = FALSE)

nc_close(orignc)
