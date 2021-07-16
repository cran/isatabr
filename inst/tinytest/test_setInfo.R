## Get location to external data folder.
extdataPath <- file.path(system.file("extdata", package = "isatabr"))

## Read files.
isaObject <- readISATab(path = file.path(extdataPath, "Atwell"))

## Check path.
expect_true(grepl("extdata", isaPath(isaObject)))
expect_true(grepl("Atwell", isaPath(isaObject)))

expect_error(isaPath(isaObject) <- "tst",
             "is not an existing folder on this system")
expect_silent(isaPath(isaObject) <- isaPath(isaObject))

## Check iFileName
expect_equal(iFileName(isaObject), "i_Investigation.txt")

expect_error(iFileName(isaObject) <- character(),
             "Did not find any investigation file at folder")
expect_error(iFileName(isaObject) <- c("tst", "tst"),
             "Found too many possible investigation files")
expect_error(iFileName(isaObject) <- "tst",
             "slot does not match the requirements")
expect_silent(iFileName(isaObject) <- iFileName(isaObject))

## Check oSR
isaOSR <- oSR(isaObject)
expect_inherits(isaOSR, "data.frame")
expect_equal_to_reference(isaOSR, "osr")

expect_error(oSR(isaObject) <- data.frame(),
             "Not all minimal required columns are present for oSR")
expect_silent(oSR(isaObject) <- isaOSR)

## Check invest
isaInvest <- invest(isaObject)
expect_inherits(isaInvest, "data.frame")
expect_equal_to_reference(isaInvest, "invest")

expect_error(invest(isaObject) <- data.frame(),
             "Not all minimal required columns are present for invest")
expect_silent(invest(isaObject) <- isaInvest)

## Check iPubs
isaIPubs <- iPubs(isaObject)
expect_inherits(isaIPubs, "data.frame")
expect_equal_to_reference(isaIPubs, "iPubs")

expect_error(iPubs(isaObject) <- data.frame(),
             "Not all minimal required columns are present for iPubs")
expect_silent(iPubs(isaObject) <- isaIPubs)

## Check iContacts
isaIContacts <- iContacts(isaObject)
expect_inherits(isaIContacts, "data.frame")
expect_equal_to_reference(isaIContacts, "iContacts")

expect_error(iContacts(isaObject) <- data.frame(),
             "Not all minimal required columns are present for iContacts")
expect_silent(iContacts(isaObject) <- isaIContacts)

## Check study
isaStudy <- study(isaObject)
expect_inherits(isaStudy, "list")
expect_equal_to_reference(isaStudy, "study")

expect_error(study(isaObject) <- list(tst = data.frame()),
             "Not all minimal required columns are present for study tst in study")
expect_silent(study(isaObject) <- isaStudy)

## Check sDD
isaSDD <- sDD(isaObject)
expect_inherits(isaSDD, "list")
expect_equal_to_reference(isaSDD, "sDD")

expect_error(sDD(isaObject) <- list(tst = data.frame()),
             "Not all minimal required columns are present for study tst in sDD")
expect_silent(sDD(isaObject) <- isaSDD)

## Check sPubs
isaSPubs <- sPubs(isaObject)
expect_inherits(isaSPubs, "list")
expect_equal_to_reference(isaSPubs, "sPubs")

expect_error(sPubs(isaObject) <- list(tst = data.frame()),
             "Not all minimal required columns are present for study tst in sPubs")
expect_silent(sPubs(isaObject) <- isaSPubs)

## Check sFacts
isaSFacts <- sFacts(isaObject)
expect_inherits(isaSFacts, "list")
expect_equal_to_reference(isaSFacts, "sFacts")

expect_error(sFacts(isaObject) <- list(tst = data.frame()),
             "Not all minimal required columns are present for study tst in sFacts")
expect_silent(sFacts(isaObject) <- isaSFacts)

## Check sAssays
isaSAssays <- sAssays(isaObject)
expect_inherits(isaSAssays, "list")
expect_equal_to_reference(isaSAssays, "sAssays")

expect_error(sAssays(isaObject) <- list(tst = data.frame()),
             "Not all minimal required columns are present for study tst in sAssays")
expect_silent(sAssays(isaObject) <- isaSAssays)

## Check sProts
isaSProts <- sProts(isaObject)
expect_inherits(isaSProts, "list")
expect_equal_to_reference(isaSProts, "sProts")

expect_error(sProts(isaObject) <- list(tst = data.frame()),
             "Not all minimal required columns are present for study tst in sProts")
expect_silent(sProts(isaObject) <- isaSProts)

## Check sContacts
isaSContacts <- sContacts(isaObject)
expect_inherits(isaSContacts, "list")
expect_equal_to_reference(isaSContacts, "sContacts")

expect_error(sContacts(isaObject) <- list(tst = data.frame()),
             "Not all minimal required columns are present for study tst in sContacts")
expect_silent(sContacts(isaObject) <- isaSContacts)

## Check sFiles
isaSFiles <- sFiles(isaObject)
expect_inherits(isaSFiles, "list")
expect_equal_to_reference(isaSFiles, "sFiles")

expect_error(sFiles(isaObject) <- list(tst = data.frame()),
             "Not all minimal required columns are present for study tst in sFiles")
expect_silent(sFiles(isaObject) <- isaSFiles)

## Check aFiles
isaAFiles <- aFiles(isaObject)
expect_inherits(isaAFiles, "list")
expect_equal_to_reference(isaAFiles, "aFiles")

expect_silent(aFiles(isaObject) <- isaAFiles)
