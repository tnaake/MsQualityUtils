## create list if Spectra

## type == "MetIDQ"
path <- system.file("metidq", package = "MsQualityUtils")
suppressWarnings(
    sps_metidq <- createListOfSpectra(type = "MetIDQ", path = path, sheet = 1))
 
## type == "mzML"
path <- system.file("sciex", package = "msdata")
suppressWarnings(sps_mzml <- createListOfSpectra(type = "mzML", path = path))

## START unit test createListOfSpectra ## 
test_that("createListOfSpectra", {
    expect_equal(length(sps_metidq), 2)
    expect_equal(length(sps_mzml), 1)
    expect_true(is.list(sps_metidq))
    expect_true(is.list(sps_mzml))
    
    expect_equal(length(sps_metidq[[1]]), 50903)
    expect_equal(length(sps_metidq[[2]]), 8876)
    expect_equal(as.numeric(table(is.na(sps_metidq[[1]]$rtime))), c(8876, 42027))
    expect_equal(as.numeric(table(is.na(sps_metidq[[2]]$rtime))), 8876)
    expect_equal(length(unique(sps_metidq[[1]]$dataOrigin)), 86)
    expect_equal(length(unique(sps_metidq[[2]]$dataOrigin)), 86)
    expect_equal(length(sps_mzml[[1]]), 1862)
    expect_equal(
        as.numeric(table(is.na(sps_mzml[[1]]$rtime))),
        1862)
    expect_equal(length(unique(sps_mzml[[1]]$dataOrigin)), 2)
})
## END unit test createListOfSpectra

