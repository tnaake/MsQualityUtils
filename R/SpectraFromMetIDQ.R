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
#' (by Hagen Gegner). The mass spectrometry platform was a
#' AB Sciex Triple Quad\code{^{TM}} 6500+ LC-MS/MS System.
#' 
#' The object will be used to create a \code{Spectra} object containing only the 
#' metabolites that were separated by liquid chromatography.
#' 
#' @details 
#' Internal use in \code{createListOfSpectra}.
#' 
#' @author Thomas Naake 
#'
#' @return \code{SummarizedExperiment} object
#' 
#' @export
#' 
#' @examples
#' rt <- loadRt()
loadRt <- function() {
    path <- system.file("metidq_rt", package = "MsQualityUtils")
    file <- paste(path, "Retention_times_MetIDQ.RDS", sep = "/")
    readRDS(file)
}


#' @name createListOfSummarizedExperimentFromMetIDQ
#' 
#' @title Create list of SummarizedExperiment object from MetIDQ files
#' 
#' @description 
#' Load the xlsx files and store them as a SummarizedExperiment object in a 
#' list.
#' 
#' @details 
#' For each xlsx file found in path a new list entry will be created. 
#' 
#' @param path \code{character}
#' @param sheet \code{character} or \code{numeric}, the name or index to read 
#' data from
#' @param ... additional parameters given to \code{read.xlsx}
#' 
#' @author Thomas Naake 
#' 
#' @importFrom MatrixQCvisUtils biocrates
#' 
#' @export
#' 
#' @examples
#' path <- system.file("metidq", package = "MsQualityUtils")
#' createListOfSummarizedExperimentFromMetIDQ(path = path, sheet = 1)
createListOfSummarizedExperimentFromMetIDQ <- function(path = "./", 
    sheet = 1, ...) {
    
    ## create an object that stores the experiments (each xlsx file is one 
    ## experiment)
    experiments <- list.files(path = path, pattern = ".xlsx", 
        full.names = TRUE, recursive = FALSE)
    
    ## create an empty list
    se_l <- list()
    
    ## iterate through the xlsx files and write the SummarizedExperiment objects
    ## to the list
    for (i in seq_along(experiments)) {
        args <- list(file = experiments[i], sheet = sheet, ...)
        se_l[[i]] <- do.call(MatrixQCvisUtils::biocrates, args)
    }
    
    ## return the list
    se_l
}


#' @name createSpectraFromSummarizedExperiment
#' 
#' @title Create Spectra object from a SummarizedExperiment object
#' 
#' @description 
#' Function to create a \code{Spectra} object from a \code{SummarizedExperiment}
#' object.
#' 
#' @details 
#' The sample name (\code{colnames(se)}) is taken to create the 
#' \code{dataOrigin} that is in down-stream functions used to distinguish the 
#' origins of the resulting \code{Spectra}'s entries.
#' 
#' The \code{Spectra} object only contains the the shared features between
#' \code{se} and \code{rt}.
#' 
#' @param se \code{SummarizedExperiment} object
#' @param rt \code{SummarizedExperiment} object containing the retention time 
#' in the assay slot
#' 
#' @author Thomas Naake 
#' 
#' @importFrom SummarizedExperiment assay
#' @importFrom Spectra Spectra
#' @importFrom S4Vectors DataFrame
#' 
#' @export
#' 
#' @examples
#' ## create rt object
#' rt <- loadRt()
#' 
#' ## create se object
#' path <- system.file("metidq", package = "MsQualityUtils")
#' se_l <- createListOfSummarizedExperimentFromMetIDQ(path = path, sheet = 1)
#' se <- se_l[[1]]
#' 
#' ## run createSpectraFromSummarizedExperiment
#' createSpectraFromSummarizedExperiment(se = se, rt = rt)
createSpectraFromSummarizedExperiment <- function(se, rt) {
    
    ## define overlap between rt and se and truncate se and rt
    sharedFeat <- intersect(rownames(se), rownames(rt))
    se <- se[sharedFeat, ]
    rt <- rt[sharedFeat, ]
    
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
        
        ## create the intensity, rtime, precursorIntensity 
        spd$intensity <- lapply(seq_len(length(m_i)), 
                                function(x) as.vector(m_i[x]))
        sps_i <- Spectra::Spectra(spd)
        sps_i$rtime <- apply(
            SummarizedExperiment::assay(rt)[names(m_i), ], 1, mean, 
            na.rm = TRUE)
        sps_i$precursorIntensity <- as.vector(m_i)
        ## for the dataOrigin take the sample name, taken from se
        sps_i$dataOrigin <- rep(colnames(se)[i], length(names(m_i)))
        
        ## write the sps_i to the list sps
        sps[[i]] <- sps_i
        names(sps)[i] <- colnames(se)[i]
    }
    sps <- Reduce(c, sps)
    sps
}

#' @name createSpectraFromMetIDQ
#' 
#' @title Create Spectra object from a list of SummarizedExperiment objects
#' 
#' @description 
#' The function \code{createSpectraFromMetIDQ} will a \code{Spectra} object
#' from list of \code{SummarizedExperiment} objects.
#' 
#' @details 
#' \code{createSpectraFromMetIDQ} will stop if there are duplicate sample 
#' names across the \code{SummarizedExperiment} objects.
#' 
#' @param se_l list of \code{SummarizedExperiment} objects
#' @param rt \code{SummarizedExperiment} object containing the retention time 
#' in the assay slot
#' 
#' @author Thomas Naake 
#' 
#' @export
#' 
#' @importFrom methods is
#' 
#' @examples
#' ## create rt object
#' rt <- loadRt()
#' 
#' ## create se object
#' path <- system.file("metidq", package = "MsQualityUtils")
#' se_l <- createListOfSummarizedExperimentFromMetIDQ(path = path, sheet = 1)
#' 
#' ## run the createSpectraFromMetIDQ
#' createSpectraFromMetIDQ(se_l = se_l, rt = rt)
createSpectraFromMetIDQ <- function(se_l, rt) {
    
    ## first, check if all elements of se_l are SummarizedExperiment objects
    .is <- lapply(se_l, function(x) methods::is(x, "SummarizedExperiment"))
    if (!all(unlist(.is)))
        stop("se_l contains elements that are not 'SummarizedExperiment'")
    
    ## second, check if the sample names across the different experiments (j) 
    ## are unique, i.e. the colnames of the SummarizedExperiment objects are 
    ## unique
    sample_names <- lapply(se_l, function(x) unique(colnames(x)))
    sample_names <- unlist(sample_names)
    if (any(duplicated(sample_names))) 
        stop("duplicated sample names across experiments")
    
    sps_l <- list()
    ## create the Spectra from the individual SummarizedExperiment 
    ## objects stored in the list se_l and write to the entries of sps
    for (i in seq_along(se_l)) {
        sps_l[[i]] <- createSpectraFromSummarizedExperiment(se_l[[i]], rt)
    }
    
    ## collapse the Spectra object of sps_l (sps_l is a list)
    sps <- Reduce(c, sps_l)
    sps
}

