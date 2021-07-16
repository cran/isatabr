## ----setup, include = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.dim = c(7, 4)
)
library(isatabr)
op <- options(width = 100)

## ----read-----------------------------------------------------------------------------------------
## Read ISA-Tab files from directory.
isaObject1 <- readISATab(path = file.path(system.file("extdata/Atwell", package = "isatabr")))

## ----readZip--------------------------------------------------------------------------------------
## Read ISA-Tab files from directory.
isaObject2 <- readISATab(path = file.path(system.file("extdata", package = "isatabr")),
                         zipfile = "Atwell.zip")

## ----pathAccess-----------------------------------------------------------------------------------
## Access path for isaObjects
isaPath(isaObject1)
isaPath(isaObject2)

## ----studyAccess----------------------------------------------------------------------------------
## Access studies.
isaStudies <- study(isaObject1)

## Print study names.
names(isaStudies)

## Access study descriptors.
isaSDD <- sDD(isaObject1)

## Shows study descriptors for study GMI_Atwell_study.
isaSDD$GMI_Atwell_study

## ----osrAccess------------------------------------------------------------------------------------
(isaOSR <- oSR(isaObject1))

## ----osrUpdate------------------------------------------------------------------------------------
## Update version number.
isaOSR[1, "Term Source Version"] <- 24

## Update oSR in ISA object.
oSR(isaObject1) <- isaOSR

## Check the updated oSR.
oSR(isaObject1)

## ----getAssay-------------------------------------------------------------------------------------
## Inspect assay tab.
isaAFile <- aFiles(isaObject1)
head(isaAFile$a_study1.txt)

## ----procAssay------------------------------------------------------------------------------------
## Get assay tabs for isaObject1.
aTabObjects <- getAssayTabs(isaObject1)

## Process assay data.
isaDat <- processAssay(isaObject = isaObject1,
                       aTabObject = aTabObjects$s_study1.txt$a_study1.txt,
                       type = "derived")

## Display first rows and columns.
head(isaDat[, 1:10])

## ----readFaahko-----------------------------------------------------------------------------------
## Read ISA-Tab files for faahKO.
isaObject3 <- readISATab(path = file.path(system.file("extdata/faahKO", package = "isatabr")))

## ----processFaahko, message=FALSE, eval=requireNamespace("xcms")----------------------------------
## Get assay tabs for isaObject3.
aTabObjects3 <- getAssayTabs(isaObject3)

## Process assay data.
isaDat3 <- processAssay(isaObject = isaObject3,
                        aTabObject = aTabObjects3$s_Proteomic_profiling_of_yeast.txt$a_metabolite.txt,
                        type = "raw")

## Display output.
isaDat3

## ----write, eval=FALSE----------------------------------------------------------------------------
#  ## Write content of ISA object to a temporary directory.
#  writeISAtab(isaObject = isaObject1,
#              path = tempdir())

## ----writeSub, eval=FALSE-------------------------------------------------------------------------
#  ## Write investigation file.
#  writeInvestigationFile(isaObject = isaObject1,
#                         path = tempdir())
#  
#  ## Write study file.
#  writeStudyFiles(isaObject = isaObject1,
#                  studyFilenames = "s_study1.txt",
#                  path = tempdir())
#  
#  ## Write assay file.
#  writeAssayFiles(isaObject = isaObject1,
#                  assayFilenames = "a_study1.txt",
#                  path = tempdir())

## ----winddown, include = FALSE------------------------------------------------
options(op)

