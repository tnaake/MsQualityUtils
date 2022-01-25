## START unit test createSpectraFromMzML ##
## SCIEX files 
path <- system.file("sciex", package = "msdata")
sps <- createSpectraFromMzML(path = path)

## SRM files
path <- system.file("srm", package = "MsQualityUtils")
sps_srm <- createSpectraFromMzML(path = path, type = "SRM")

test_that("createSpectraFromMzML", {
    ## SCIEX files
    expect_is(sps, "Spectra")
    expect_equal(length(sps), 1862)
    expect_equal(as.vector(table(is.na(sps$rtime))), 1862)
    expect_equal(length(unique(sps$dataOrigin)), 2)
    
    ## SRM files
    expect_is(sps_srm, "Spectra")
    expect_equal(length(sps_srm), 24964)
    expect_equal(as.vector(table(is.na(sps_srm$rtime))), 24964)
    expect_equal(length(unique(sps_srm$dataOrigin)), 1)
})
## END unit test createSpectraFromMzML

## START unit test createSpectraFromSrmOrMrmMzML
path <- system.file("srm", package = "MsQualityUtils")
sps_srm <- createSpectraFromSrmOrMrmMzML(path = path)

test_that("createSpectraFromSrmOrMrmMzML", {
    expect_is(sps_srm, "Spectra")
    expect_equal(length(sps_srm), 24964)
    expect_equal(as.vector(table(is.na(sps_srm$rtime))), 24964)
    expect_equal(length(unique(sps_srm$dataOrigin)), 1)
})
## END unit test createSpectraFromSrmOrMrmMzML
