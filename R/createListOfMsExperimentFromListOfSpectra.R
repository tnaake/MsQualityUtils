#' @name createListOfMsExperimentFromListOfSpectra
#' 
#' @title Create list of MsExperiment objects from list of Spectra objects
#' 
#' @description 
#' The function \code{createListOfMsExperimentFromListOfSpectra} returns a 
#' list of \code{MsExperiment} objects. 
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
#' @param sps_l \code{list} of \code{Spectra} objects
#' 
#' @author Thomas Naake 
#' 
#' @importFrom MsExperiment MsExperiment `sampleData<-` sampleData `spectra<-` 
#'     linkSampleData 
#' 
#' @examples
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
