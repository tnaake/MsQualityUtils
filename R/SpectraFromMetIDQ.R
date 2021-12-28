#' @name loadRt
#' 
#' @title Load the file with the retention time information
#' 
#' @description 
#' The function \code{loadRt} loads the Retention_times_MetIDQ object stored in
#' \code{"Retention_times_MetIDQ.RDS"}. 
#' 
#' \code{Retention_times_MetIDQ} contains the retention time
#' in seconds of the metabolites for the Biocrates MxP 500 Quant Kit measured 
#' by the Metabolomics Core Technology Platorm, COS, University of Heidelberg
#' (by Hagen Gegner).
#' It will be used to create a \code{Spectra} list containing only the 
#' metabolites that were separated by liquid chromatography.
#' 
#' @details 
#' Internal use in \code{createListOfSpectra}.
#' 
#' @author Thomas Naake 
#'
#' @return \code{SummarizedExperiment} object
#' 
#' @examples
#' Retention_times_MetIDQ <- loadRt()
#' 
loadRt <- function() {
    readRDS("Retention_times_MetIDQ.RDS")
}


#' @name createListOfSummarizedExperimentFromMetIDQ
#' 
#' @title Create list of SummarizedExperiment object from MetIDQ files
#' 
#' @description 
#' Load the xlsx files and store them as a SummarizedExperiment object in a list.
#' 
#' @details 
#' 
#' @param path \code{character}
#' @param sheet \code{character} or \code{numeric}, the name or index to read 
#' data from
#' @param ... additional parameters given to \code{read.xlsx}
#' 
#' @author Thomas Naake 
#' 
#' @importFrom MatrixQCvis biocrates
#' 
#' @examples
#' path <- system.file("metidq", package = "MsQualityUtils")
#' createListOfSummarizedExperimentFromMetIDQ(path = path, sheet = 1)
createListOfSummarizedExperimentFromMetIDQ <- function(path = "./", sheet = 1, ...) {
    
    ## create an object that stores the experiments (each xlsx file is one 
    ## experiment)
    experiments <- list.files(path = path, pattern = ".xlsx", full.names = TRUE)
    
    ## create an empty list
    se_l <- list()
    
    ## iterate through the xlsx files and write the SummarizedExperiment objects
    ## to the list
    for (i in seq_along(experiments)) {
        se_l[[i]] <- MatrixQCvis::biocrates(file = experiments[i], 
            sheet = sheet, ...)
    }
    
    ## return the list
    se_l
}


#' @name createSpectraFromSummarizedExperiment
#' 
#' @title Create Spectra object from a SummarizedExperiment object
#' 
#' @description 
#' Outer function to iterate through list of SummarizedExperiment objects.
#' 
#' @details 
#' 
#' @param se \code{SummarizedExperiment}
#' @param rt \code{SummarizedExperiment} containing the retention time in the 
#' assay slot
#' 
#' @author Thomas Naake 
#' 
#' @importFrom SummarizedExperiment assay
#' @importFrom Spectra Spectra
#' @importFrom S4Vectors DataFrame
#' 
#' @examples
#' 
createSpectraFromSummarizedExperiment <- function(se, rt) {
    ## create the Spectra object from the SummarizedExperiment object
    a <- SummarizedExperiment::assay(se)
    sps <- list()
    for (i in 1:ncol(a)) {
        m_i <- a[!is.na(a[, i]), i]
        spd <- S4Vectors::DataFrame(
            msLevel = c(rep(1L, length(m_i))),
            polarity = c(rep(1L, length(m_i))),
            id = names(m_i),
            name = names(m_i))
        
        ## fake m/z values
        spd$mz <- lapply(seq_len(length(m_i)), function(x) x)
        
        ## enter here the
        spd$intensity <- lapply(seq_len(length(m_i)), 
                                function(x) as.vector(m_i[x]))
        sps_i <- Spectra::Spectra(spd)
        sps_i$rtime <- apply(assay(rt)[names(m_i), ], 1, mean, na.rm = TRUE)
        sps_i$precursorIntensity <- as.vector(m_i)
        sps_i$dataOrigin <- rep(colnames(s)[i], length(names(m_i)))
        sps[[i]] <- sps_i
        names(sps)[i] <- colnames(s)[i]
    }
    sps <- Reduce(c, sps)
    sps
}

#' @name createSpectraFromMetIDQ
#' 
#' @title Create Spectra object from a list of SummarizedExperiment objects
#' 
#' @description 
#' 
#' @details 
#' 
#' @param se_l list of \code{SummarizedExperiment} objects
#' @param rt
#' 
#' @author Thomas Naake 
#' 
#' @examples
#' 
createSpectraFromMetIDQ <- function(se_l, rt) {
    
    ## first, check if all elements of se_l are SummarizedExperiment objects
    .is <- lapply(se_l, function(x) is(x, "SummarizedExperiment"))
    if (!all(unlist(.is)))
        stop("se_l contains elements that are not 'SummarizedExperiment'")
    
    ## second, check if the sample names across the different experiments (j) 
    ## are unique, i.e. the colnames of the SummarizedExperiment objects are 
    ## unique
    sample_names <- lapply(se_l, function(x) unique(colnames(x)))
    sample_names <- unlist(sample_names)
    if (any(duplicated(sample_names))) 
        stop("duplicated sample names across experiments")
    
    sps <- list()
    ## create the Spectra from the individual SummarizedExperiment 
    ## objects stored in the list se_l and write to the entries of sps
    for (i in seq_along(se_l)) {
        sps[[i]] <- createSpectraFromSummarizedExperiment(se_l[[i]], rt)
    }
    
    ## collapse the Spectra object of sps_outer (sps_outer is a list)
    sps <- Reduce(c, sps)
    sps
}

