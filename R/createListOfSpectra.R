#' @name createListOfSpectra
#' 
#' @title Create list of Spectra object(s) 
#' 
#' @description 
#' The function \code{createListOfSpectra} returns a list of \code{Spectra} 
#' objects.
#' 
#' When \code{type == "MetIDQ"}, the function will return a list of length 2. 
#' The first entry contains a \code{Spectra} object with all features found in 
#' the experiment files. The second entry contains a \code{Spectra} object with 
#' the features found in the experiment files that have available information on 
#' their retention time.
#' 
#' When \code{type == "mzML"}, the function will return a lit of length 1. The
#' entry contains a \code{Spectra} object derived from all mzML files in 
#' \code{path}.
#' 
#' @details  
#' If \code{type == "MetIDQ"}, the argument \code{rt} can be passed to the 
#' function. \code{rt} has to be a \code{SummarizedExperiment} object 
#' containing retention time values in the assay slot. By default, 
#' \code{createListOfSpectra} loads
#' an in-house library of retention time values from the metabolites of the
#' Biocrates MxP Quant 500 kit that are separated by liquid chromatography. 
#' The \code{Spectra} object only contains the the shared features between
#' \code{se} and \code{rt}. 
#' 
#' @param type \code{character}, either \code{"MetIDQ"} or \code{"mzML"}
#' @param ... arguments passed to 
#' \code{createListOfSummarizedExperimentFromMetIDQ}
#' or \code{createSpectraFromMzML}, for instance \code{path}
#' 
#' @author Thomas Naake 
#' 
#' @return list of \code{Spectra} object(s)
#' 
#' @importFrom ProtGenerics rtime
#' 
#' @export
#' 
#' @examples
#' ## type == "MetIDQ"
#' path <- system.file("metidq", package = "MsQualityUtils")
#' createListOfSpectra(type = "MetIDQ", path = path, sheet = 1)
#' 
#' ## type == "mzML"
#' path <- system.file("sciex", package = "msdata")
#' createListOfSpectra(type = "mzML", path = path)
createListOfSpectra <- function(type = c("MetIDQ", "mzML"), ...) {
    
    type <- match.arg(type)
    
    ## create a list from the ... arguments that is later passed to do.call
    args <- list(...)
    
    if (type == "MetIDQ") {
        
        se_l <- do.call("createListOfSummarizedExperimentFromMetIDQ", args)
        
        ## load the rt file
        if (!("rt" %in% names(args)))
            rt <- loadRt()
        
        sps <- createSpectraFromMetIDQ(se_l = se_l, rt = rt)
        
        ## filter the sps object that it only contains features that have an 
        ## associated retention time, these are the features that are separated by 
        ## liquid chromatography
        sps_lc <- sps[!is.na(ProtGenerics::rtime(sps)), ]
    
        sps_l <- list(sps, sps_lc)    
    }
    
    if (type == "mzML") {
        
        sps <- do.call("createSpectraFromMzML", args)
        sps_l <- list(sps)
    }
    
    sps_l
}
