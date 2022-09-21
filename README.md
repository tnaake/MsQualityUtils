# MsQualityUtils

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
![R-CMD-check-bioc](https://github.com/tnaake/MsQualityUtils/workflows/R-CMD-check-bioc/badge.svg)
[![codecov.io](http://codecov.io/github/tnaake/MsQualityUtils/coverage.svg?branch=master)](http://codecov.io/github/tnaake/MsQualityUtls?branch=main)
[![license](http://img.shields.io/badge/license-GPL%20%28%3E=%203%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)

Creation of MsExperiment objects for MsQuality-based QC metric calculation

## Description
Data quality assessment is an integral part of preparatory data analysis 
to ensure sound biological information retrieval. 

We present here the `MsQualityUtils` package, which provides functionality to create
`MsExperiment` objects from mzML and MetIDQ-derived files. 

The `MsExperiment` files can subsequently be used to calculate quality metrics 
at the per-sample level using the 
[`MsQuality`](https://github.com/tnaake/MsQuality) package. 

`MsQuality` relies on the [`mzQC`](https://github.com/HUPO-PSI/mzQC) 
framework of quality metrics defined by the Human Proteome 
Organization-Proteomics Standards Intitiative (HUPO-PSI). 

The `MsQuality` package is built upon the `Spectra` and the `MsExperiment` package.
Metrics will be calculated based on the information stored in a 
`Spectra` object, thus, the spectral data of each sample should be stored
in one `Spectra` object. The `MsExperiment` serves as a container to 
store the mass spectral data of multiple samples. `MsQuality` enables the user
to calculate quality metrics both on `Spectra` and `MsExperiment` objects. 
The `MsQualityUtils` package provides functionality to create `MsExperiment` 
objects that store the (spectral) information of mzML or MetIDQ files. 

## Contact 

You are welcome to 

 * write a mail to <thomasnaake (at) googlemail (dot) com> 
 * submit suggestions and issues: <https://github.com/tnaake/MsQualityUtils/issues>
 * send a pull request: <https://github.com/tnaake/MsQualityUtils/issues> 

## Install
To install `MsQualityUtils`, you have first to install the 
[devtools](http://cran.r-project.org/web/packages/devtools/index.html) package: 

```r
install.packages("devtools")
library("devtools")
```

Install the `MsQualityUtils` package then via
```r
install_github("tnaake/MsQualityUtils")
```


