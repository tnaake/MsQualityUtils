
## START unit test loadRt ##
rt <- loadRt()
test_that("loadRt", {
    expect_is(rt, "SummarizedExperiment")
    expect_equal(dim(rt), c(630, 6))
    expect_equal(
        as.vector(table(is.na(SummarizedExperiment::assay(rt)))), c(636, 3144))
    expect_equal(sum(SummarizedExperiment::assay(rt), na.rm = TRUE),
        84937.56)
})
## END unit test loadRt ## 

## START unit test createListOfSummarizedExperimentFromMetIDQ ##
path <- system.file("metidq", package = "MsQualityUtils")
suppressWarnings(se_l <- 
    createListOfSummarizedExperimentFromMetIDQ(path = path, sheet = 1))
test_that("createListOfSummarizedExperimentFromMetIDQ", {
    expect_equal(length(se_l), 1)
    expect_equal(is(se_l[[1]]), "SummarizedExperiment")
    expect_equal(dim(se_l[[1]]), c(630, 86))
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
    expect_equal(length(sps), 50903)
    expect_equal(as.vector(table(is.na(sps$rtime))), c(8876, 42027))
    expect_equal(length(unique(sps$dataOrigin)), 86)
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
    expect_equal(length(sps), 50903)
    expect_equal(as.vector(table(is.na(sps$rtime))), c(8876, 42027))
    expect_equal(length(unique(sps$dataOrigin)), 86)
})
## END unit test createSpectraFromMetIDQ ##
