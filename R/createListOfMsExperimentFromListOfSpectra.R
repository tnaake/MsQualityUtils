#' @name createListOfMsExperimentFromListOfSpectra
#' 
#' @title Create list of \code{MsExperiment} objects from list of 
#' \code{Spectra} objects
#' 
#' @description
#' The function \code{createListOfMsExperimentFromListOfSpectra} will 
#' provide the input (after subsetting the resulting list) for the 
#' \code{MsQuality} workflow for QC metric calculation.
#' 
#' The function \code{createListOfMsExperimentFromListOfSpectra} returns a 
#' list of \code{MsExperiment} objects. It will take as input a list of 
#' \code{Spectra} objects.
#' 
#' @details
#' When \code{type == "MetIDQ"}, the function will return a list of length 2. 
#' The first entry contains a \code{MsExperiment} object with all features found 
#' in the experiment files. The second entry contains a \code{MsExperiment} 
#' object with the features found in the experiment files that have available 
#' information on their retention time.
#' 
#' When \code{type == "mzML"}, the function will return a lit of length 1. The
#' entry contains a \code{MsExperiment} object derived from all mzML files in 
#' \code{path}.
#'  
#' @param sps_l \code{list} of one or multiple \code{Spectra} objects
#' 
#' @author Thomas Naake 
#' 
#' @importFrom MsExperiment MsExperiment `sampleData<-` sampleData `spectra<-` 
#'     linkSampleData 
#' 
#' @export
#' 
#' @examples
#' ## type == "MetIDQ"
#' path <- system.file("metidq", package = "MsQualityUtils")
#' sps_l <- createListOfSpectra(type = "MetIDQ", path = path, sheet = 1)
#' createListOfMsExperimentFromListOfSpectra(sps_l = sps_l)
#' 
#' ## type == "mzML"
#' path <- system.file("sciex", package = "msdata")
#' sps_l <- createListOfSpectra(type = "mzML", path = path)
#' createListOfMsExperimentFromListOfSpectra(sps_l = sps_l)
#' 
createListOfMsExperimentFromListOfSpectra <- function(sps_l) {
    
    msexp_l <- lapply(sps_l, function(sps) {
        ## create an empty MsExperiment object
        msexp <- MsExperiment::MsExperiment()
        
        ## fill it with data
        samples <- unique(sps$dataOrigin)
        MsExperiment::sampleData(msexp) <- S4Vectors::DataFrame(samples = samples)
        rownames(MsExperiment::sampleData(msexp)) <- samples
        MsExperiment::spectra(msexp) <- sps
        
        ## link the spectra to the samples
        msexp <- MsExperiment::linkSampleData(object = msexp,
            with = "sampleData.samples = spectra.dataOrigin")
    })
    
    msexp_l 
}
