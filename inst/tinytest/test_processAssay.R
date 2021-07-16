## Get location to external data folder.
extdataPath <- file.path(system.file("extdata", package = "isatabr"))

## Read Atwell files.
isaObject <- readISATab(path = file.path(extdataPath, "Atwell"))

## Get assay tabs for isaObject.
aTabObjects <- getAssayTabs(isaObject)

expect_equal(names(aTabObjects), "s_study1.txt")
expect_equal(names(aTabObjects$s_study1.txt), "a_study1.txt")
expect_inherits(aTabObjects$s_study1.txt$a_study1.txt, "assayTab")

isaDat <- processAssay(isaObject = isaObject,
                       aTabObject = aTabObjects$s_study1.txt$a_study1.txt,
                       type = "derived")

expect_inherits(isaDat, "data.frame")

## Read faahKO files.
isaObject3 <- readISATab(path = file.path(extdataPath, "faahKO"))

## Get assay tabs for isaObject.
aTabObjects3 <- getAssayTabs(isaObject3)

expect_inherits(aTabObjects3$s_Proteomic_profiling_of_yeast.txt$a_metabolite.txt, "msAssayTab")

## Processing requires xcms. Skip on CRAN.
if (at_home()) {
  isaDat3 <- processAssay(isaObject = isaObject3,
                          aTabObject = aTabObjects3[[1]][[1]],
                          type = "raw")

  expect_inherits(isaDat3, "xcmsSet")
}
