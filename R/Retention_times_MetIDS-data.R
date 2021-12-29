#' @name Retention_times_MetIDQ
#' 
#' @title Data for \code{MsQualityUtils}: Retention time for MetIDQ
#' 
#' @description \code{Retention_times_MetIDQ} contains the retention time
#' in seconds of the metabolites for the Biocrates MxP 500 Quant Kit measured 
#' by the Metabolomics Core Technology Platorm, COS, University of Heidelberg
#' (by Hagen Gegner).
#' It will be used to create a `Spectra` list containing only the metabolites
#' that were separated by liquid chromatography.
#' 
#' @docType data
#' 
#' @return \code{SummarizedExperiment}
#' 
#' @format \code{SummarizedExperiment}
#' 
#' @source
#' library(MatrixQCvis)
#' rt <- biocrates(file = "Retention_times_MetIDQ.xlsx", sheet = "2021_06_15_RT")
#' 
#' ## convert to seconds
#' rt <- MatrixQCvis:::updateSE(se = rt, assay(rt) * 60)
#' 
#' saveRDS(rt, "Retention_times_MetIDQ.RDS")
#'
#' @author Thomas Naake
NULL