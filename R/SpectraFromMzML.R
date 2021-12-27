#' @name createSpectraFromMzML
#' 
#' @title Create Spectra object from mzML files
#' 
#' @description
#' The function \code{createSpectraFromMzML} creates a (single) \code{Spectra} 
#' object from all mzML files found in \code{path}.
#' 
#' @details 
#' The package uses the \code{MsBackendMzR} for importing mzML files. The entries 
#' in the returned \code{Spectra} object are separable by 
#' \code{Spectra$dataOrigin}.
#' 
#' @param path \code{character}
#' 
#' @author Thomas Naake 
#' 
#' @return \code{Spectra} object
#' 
#' @importFrom Spectra Spectra MsBackendMzR
#' 
#' @examples
#' path <- system.file("sciex", package = "msdata")
#' createSpectraFromMzML(path = path)
createSpectraFromMzML <- function(path = ".") {
    
    fls <- dir(path, pattern = "[.]mzML", full.names = TRUE)
    sps <- Spectra::Spectra(fls, backend = Spectra::MsBackendMzR())
    sps
}

