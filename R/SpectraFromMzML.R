#' @name createSpectraFromMzML
#' 
#' @title Create Spectra object from mzML files
#' 
#' @description
#' The function \code{createSpectraFromMzML} creates a (single) \code{Spectra} 
#' object from all mzML files found in \code{path}.
#' 
#' @details
#' The functions imports mzML files by either \code{Spectra::Spectra} and the
#' \code{MsBackendMzR} backend or \code{MSnbase::readSRMData}. 
#' The entries in the returned \code{Spectra} object are separable by 
#' \code{Spectra$dataOrigin}.
#' 
#' The \code{type} argument specifies the underlying technology of the 
#' mzML files. Depending on the technology the quantitative information will 
#' be stored in different lists within the mzML files. In case of 
#' selected reaction monitoring (SRM)- or multiple reaction monitoring 
#' (MRM)-derived files, the intensities and m/z values are stored in 
#' \code{chromatogram} entries within \code{chromatogramList}. In this case,
#' \code{type} has to be set to \code{"SRM"} or \code{"MRM"}. Typically, 
#' the intensities and m/z values are stored in \code{spectrum} entries
#' within \code{spectrumList}. In this case, \code{type} has to be set 
#' to any other value than \code{"SRM"} or \code{"MRM"}.
#' 
#' @param path \code{character}, path to mzML files
#' @param type \code{character}, if \code{"SRM"} or \code{"MRM"} the mzML files
#' will be loaded via \code{MSnbase::readSRMData}, otherwise the 
#' mzML files will be loaded via \code{Spectra::Spectra} and the 
#' \code{MsBackendMzR()}
#' 
#' @author Thomas Naake 
#' 
#' @return \code{Spectra} object
#' 
#' @importFrom Spectra Spectra MsBackendMzR
#' 
#' @export
#' 
#' @examples
#' ## for non-SRM and non-MRM files
#' path <- system.file("sciex", package = "msdata")
#' createSpectraFromMzML(path = path)
#' 
#' ## for SRM and MRM files
#' path <- system.file("srm", package = "MsQualityUtils")
#' createSpectraFromMzML(path = path, type = "SRM")
createSpectraFromMzML <- function(path = ".", type = "") {
    
    if (type %in% c("SRM", "MRM")) {
        sps <- createSpectraFromSrmOrMrmMzML(path)
    } else {
        fls <- dir(path, pattern = "[.]mzML", full.names = TRUE)
        sps <- Spectra::Spectra(fls, backend = Spectra::MsBackendMzR())
    }
    
    
    sps
}


#' @name createSpectraFromSrmOrMrmMzML
#' 
#' @title Create Spectra object from mzML files (SRM or MRM technology)
#' 
#' @description
#' The function \code{createSpectraFromSrmOrMrmMzML} creates a (single) 
#' \code{Spectra} object from all mzML files found in \code{path}.
#' 
#' @details
#' Depending on the technology the quantitative information will 
#' be stored in different lists within the mzML files. In case of 
#' selected reaction monitoring (SRM)- or multiple reaction monitoring 
#' (MRM)-derived files, the intensities and m/z values are stored in 
#' \code{chromatogram} entries within \code{chromatogramList}.
#' The mzML files will be loaded via \code{MSnbase::readSRMData} and 
#' stored within a \code{MChromatogram} object. 
#' 
#' The entries in the returned \code{Spectra} object are separable by 
#' \code{Spectra$dataOrigin}.
#' 
#' @param path \code{character}, path to mzML files
#' 
#' @author Thomas Naake 
#' 
#' @return \code{Spectra} object
#' 
#' @importFrom MSnbase readSRMData pData
#' @importFrom S4Vectors DataFrame
#' @importFrom ProtGenerics msLevel polarity rtime precursorMz intensity 
#' @importFrom ProtGenerics productMz
#' @importFrom Spectra Spectra MsBackendDataFrame
#' 
#' @export
#' 
#' @examples
#' path <- system.file("srm", package = "MsQualityUtils")
#' createSpectraFromSrmOrMrmMzML(path = path)
createSpectraFromSrmOrMrmMzML <- function(path = ".") {
    
    fls <- dir(path, pattern = "[.]mzML", full.names = TRUE)

    ## create empty list to store Spectra objects
    sp_l <- list()

    ## iterate through files and create Spectra objects, store them in sp_l
    for (i in seq_along(fls)) {
        srm <- MSnbase::readSRMData(fls[i])
        
        ## retrieve/calculate the values for the Spectra object
        msLevelVals <- unlist(lapply(srm, ProtGenerics::msLevel))
        polarityVals <- ProtGenerics::polarity(srm)
        idVals <- rownames(srm)
        rtVals <- lapply(srm, ProtGenerics::rtime)
        precursorMzVals <- apply(ProtGenerics::precursorMz(srm), 1, mean)
        intensityVals <- lapply(srm, ProtGenerics::intensity)
        intensityVals <- unlist(intensityVals)
        mzVals <- apply(ProtGenerics::productMz(srm), 1, mean)
        dataOriginVals <- MSnbase::pData(srm)[["file"]]
        
        ## get the times argument for rep,
        reps <- unlist(lapply(rtVals, length))
        rtVals <- unlist(rtVals)
        
        ## assign msLevel, polarity, precursorMz, retention time, intensity,  
        ## and mz values
        spd <- S4Vectors::DataFrame(
            msLevel = rep(msLevelVals, times = reps),
            polarity = rep(polarityVals, times = reps),
            id = rep(idVals, times = reps),
            name = rep(idVals, times = reps),
            precursorMz = rep(precursorMzVals, times = reps),
            rtime = rtVals,
            intensity = intensityVals,
            mz = rep(mzVals, times = reps),
            dataOrigin = dataOriginVals
        )
    
        ## order the DataFrame according to mz values
        spd <- spd[order(spd$mz), ]
        sp_l[[i]] <- Spectra::Spectra(spd, 
            backend = Spectra::MsBackendDataFrame())
    }
    sps <- Reduce(c, sp_l)
    sps
}

