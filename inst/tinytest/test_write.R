## Get location to external data folder.
extdataPath <- file.path(system.file("extdata", package = "isatabr"))

## Read files.
isaObject <- readISATab(path = file.path(extdataPath, "Atwell"))

## Create tempdir for writing files.
tmpDir <- tempdir()

## Check writing full ISA object.
expect_silent(writeISAtab(isaObject, path = tmpDir))

## Reread - check that everything except path is identical.
isaObject2 <- readISATab(tmpDir)

## To simplify comparison set path for isaObject2 to that of isaObject.
isaPath(isaObject2) <- isaPath(isaObject)

expect_identical(isaObject, isaObject2)

## Check writing investigation file.
expect_silent(writeInvestigationFile(isaObject, path = tmpDir))

## Check writing study files.
expect_error(writeStudyFiles(isaObject,
                             studyFilenames = "tst",
                             path = tmpDir),
             "The following study files are not present in the isaObject")

expect_silent(writeStudyFiles(isaObject,
                              studyFilenames = "s_study1.txt",
                              path = tmpDir))

## Check writing assay files.
expect_error(writeAssayFiles(isaObject,
                             assayFilenames = "tst",
                             path = tmpDir),
             "The following assay files are not present in the isaObject")

expect_silent(writeAssayFiles(isaObject,
                              assayFilenames = "a_study1.txt",
                              path = tmpDir))
