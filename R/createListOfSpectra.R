#' @name createListOfSpectra
#' 
#' @title Create list of Spectra object(s) 
#' 
#' @description 
#' The function `createListOfSpectra` returns a list of `Spectra` objects.
#' 
#' When `type == "MetIDQ"`, the function will return a list of length 2. The 
#' first entry contains a `Spectra` object with all features found in the 
#' experiment files. The second entry contains a `Spectra` object with the
#' features found in the experiment files that have available information on 
#' their retention time.
#' 
#' When `type == "mzML"`, the function will return a lit of length 1. The
#' entry contains a `Spectra` object derived from all mzML files in `path`.
#' 
#' @details  
#' When `type == "MetIDQ"`, the argument `rt` can be passed to the function.
#' `rt` has to be a `SummarizedExperiment` object containing retention 
#' time values in the assay slot. By default, `createListOfSpectra` loads
#' an in-house library of retention time values from the metabolites of the
#' Biocrates MxP Quant 500 kit that are separated by liquid chromatography. 
#' 
#' @param type `character`, either "MetIDQ" or "mzML"
#' @param ... arguments passed to `createListOfSummarizedExperimentFromMetIDQ`
#' or `createSpectraFromMzML`, for instance `path`
#' 
#' @author Thomas Naake 
#' 
#' @return list of `SummarizedExperiment` object(s)
#' 
#' @importFrom ProtGenerics rtime
#' 
#' @examples
#' ## type == "MetIDQ"
#' path <- system.file("metidq", package = "MsQualityUtils")
#' createListOfSpectra(type = "MetIDQ", path = path, sheet = 1)
#' 
#' ## type == "mzML"
#' path <- system.file("sciex", package = "msdata")
#' createListOfSpectra(type = "mzML", path = path)
#' 
#' createListOfSpectra
createListOfSpectra <- function(type = c("MetIDQ", "mzML"), ...) {
    
    type <- match.arg(type)
    
    if (type == "MetIDQ") {
        se_l <- createListOfSummarizedExperimentFromMetIDQ(path = path, 
            sheet = sheet, ...)
        
        ## load the rt file
        if (!missing(rt))
            rt <- loadRt()
        
        sps <- createSpectraFromMetIDQ(se_l = se_l, rt = rt)
        
        ## filter the sps object that it only contains features that have an 
        ## associated retention time, these are the features that are separated by 
        ## liquid chromatography
        sps_lc <- sps[!is.na(ProtGenerics::rtime(sps)), ]
    
        sps_l <- list(sps, sps_lc)    
    }
    
    if (type == "mzML") {
        sps <- createSpectraFromMzML(path = path)
        sps_l <- list(sps)
    }
    
    sps_l
}
