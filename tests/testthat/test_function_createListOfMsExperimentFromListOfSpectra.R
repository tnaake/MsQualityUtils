## create list of MsExperiment object

## type == "MetIDQ"
path <- system.file("metidq", package = "MsQualityUtils")
suppressWarnings(sps_metidq <- 
    createListOfSpectra(type = "MetIDQ", path = path, sheet = 1))
msexp_metidq <- createListOfMsExperimentFromListOfSpectra(sps_l = sps_metidq)
 
## type == "mzML"
path <- system.file("sciex", package = "msdata")
suppressWarnings(sps_mzml <- createListOfSpectra(type = "mzML", path = path))
msexp_mzml <- createListOfMsExperimentFromListOfSpectra(sps_l = sps_mzml)

test_that("createListOfMsExperimentFromListOfSpectra", {
    expect_equal(length(msexp_metidq), 2)
    expect_equal(length(msexp_mzml), 1)
    expect_true(is.list(msexp_metidq))
    expect_true(is.list(msexp_mzml))
    
    expect_equal(length(spectra(msexp_metidq[[1]])), 50903)
    expect_equal(length(spectra(msexp_metidq[[2]])), 8876)
    expect_equal(
        as.numeric(table(is.na(MsExperiment::spectra(msexp_metidq[[1]])$rtime))), 
        c(8876, 42027))
    expect_equal(
        as.numeric(table(is.na(MsExperiment::spectra(msexp_metidq[[2]])$rtime))), 
        8876)
    expect_equal(
        length(unique(MsExperiment::spectra(msexp_metidq[[1]])$dataOrigin)), 86)
    expect_equal(
        length(unique(MsExperiment::spectra(msexp_metidq[[2]])$dataOrigin)), 86)
    expect_equal(length(MsExperiment::spectra(msexp_mzml[[1]])), 1862)
    expect_equal(
        as.numeric(table(is.na(MsExperiment::spectra(msexp_mzml[[1]])$rtime))), 
        1862)
    expect_equal(
        length(unique(MsExperiment::spectra(msexp_mzml[[1]])$dataOrigin)), 2)
})
## END unit test createListOfMsExperimentFromListOfSpectra
