## Check that checks work correctly.
expect_error(readISATab(path = 1),
             "The provided arguments must be of class character")
expect_error(readISATab(path = "bla"),
             "bla is not an existing folder on this system")
expect_error(readISATab(),
             "Did not find any investigation file in folder")

## Get location to external data folder.
extdataPath <- file.path(system.file("extdata", package = "isatabr"))

## Check reading of files.
expect_silent(readISATab(path = file.path(extdataPath, "Atwell")))

## Check reading of files with extra / at end of path.
pathExt <- paste0(file.path(extdataPath, "Atwell"), "/")
expect_silent(readISATab(path = pathExt))

## Check reading from zip files
expect_silent(readISATab(path = extdataPath, zipfile = "Atwell.zip"))

## Check that option verbose works correctly.
expect_message(readISATab(path = file.path(extdataPath, "Atwell"),
                          verbose = TRUE),
               "Converting ISA-Tab dataset at")

expect_message(readISATab(path = extdataPath, zipfile = "Atwell.zip",
                          verbose = TRUE),
               "Unzipping file in temporary directory")
expect_message(readISATab(path = extdataPath, zipfile = "Atwell.zip",
                          verbose = TRUE),
               "Unzipped files: a_study1.txt, i_Investigation.txt, s_study1.txt, tdf.txt")
expect_message(readISATab(path = extdataPath, zipfile = "Atwell.zip",
                          verbose = TRUE),
               "Converting ISA-Tab dataset at")
