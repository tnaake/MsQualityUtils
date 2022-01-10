## START unit test createSpectraFromMzML ##
path <- system.file("sciex", package = "msdata")
sps <- createSpectraFromMzML(path = path)

test_that("", {
    expect_is(sps, "Spectra")
    expect_equal(length(sps), 1862)
    expect_equal(as.vector(table(is.na(sps$rtime))), 1862)
    expect_equal(length(unique(sps$dataOrigin)), 2)
})
## END unit test createSpectraFromMzML
