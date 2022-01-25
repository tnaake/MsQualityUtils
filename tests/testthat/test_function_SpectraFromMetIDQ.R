
## START unit test loadRt ##
rt <- loadRt()
test_that("loadRt", {
    expect_is(rt, "SummarizedExperiment")
    expect_equal(dim(rt), c(630, 6))
    expect_equal(
        as.vector(table(is.na(SummarizedExperiment::assay(rt)))), c(636, 3144))
    expect_equal(sum(SummarizedExperiment::assay(rt), na.rm = TRUE),
        1415.626)
})
## END unit test loadRt ## 

## START unit test createListOfSummarizedExperimentFromMetIDQ ##
path <- system.file("metidq", package = "MsQualityUtils")
suppressWarnings(se_l <- 
    createListOfSummarizedExperimentFromMetIDQ(path = path, sheet = 1))
test_that("createListOfSummarizedExperimentFromMetIDQ", {
    expect_equal(length(se_l), 1)
    expect_is(se_l[[1]], "SummarizedExperiment")
    expect_equal(dim(se_l[[1]]), c(408, 504))
}) 
## END unit createListOfSummarizedExperimentFromMetIDQ ## 

## START unit test createSpectraFromSummarizedExperiment ##
## create rt object
rt <- loadRt()
 
## create se object
path <- system.file("metidq", package = "MsQualityUtils")
suppressWarnings(se_l <- 
    createListOfSummarizedExperimentFromMetIDQ(path = path, sheet = 1))
se <- se_l[[1]]
 
## run createSpectraFromSummarizedExperiment
sps <- createSpectraFromSummarizedExperiment(se = se, rt = rt)

test_that("createSpectraFromSummarizedExperiment", {
    expect_is(sps, "Spectra")
    expect_equal(length(sps), 22076)
    expect_equal(as.vector(table(is.na(sps$rtime))), c(16525, 5551))
    expect_equal(length(unique(sps$dataOrigin)), 504)
})
## END unit test createSpectraFromSummarizedExperiment ##


## START unit test createSpectraFromMetIDQ ##
## create rt object
rt <- loadRt()
 
## create se object
path <- system.file("metidq", package = "MsQualityUtils")
suppressWarnings(se_l <- 
    createListOfSummarizedExperimentFromMetIDQ(path = path, sheet = 1))
 
## run the createSpectraFromMetIDQ
sps <- createSpectraFromMetIDQ(se_l = se_l, rt = rt)

test_that("createSpectraFromMetIDQ", {
    expect_is(sps, "Spectra")
    expect_equal(length(sps), 22076)
    expect_equal(as.vector(table(is.na(sps$rtime))), c(16525, 5551))
    expect_equal(length(unique(sps$dataOrigin)), 504)
})
## END unit test createSpectraFromMetIDQ ##
