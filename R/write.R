#' Write ISA-Tab object.
#'
#' Write ISA-Tab object to files. The investigation file, study files and assay
#' files are written to the folder specified in \code{path}.\cr\cr
#' It is also possible to write only the investigation file, one or more
#' study files, or one or more assay files using the respective functions.
#'
#' @param isaObject An object of the \code{\link{ISA-class}}.
#' @param path A character vector with the name of the directory to which the
#' file(s) should be written. The default value is the current working
#' directory.
#'
#' @return No return value, files are written to path.
#'
#' @examples
#' ## Read example Atwell data set.
#' isaObject1 <- readISATab(path = file.path(system.file("extdata/Atwell",
#'                                           package = "isatabr")))
#'
#' ## Write content of ISA object to a temporary directory.
#' writeISAtab(isaObject = isaObject1,
#'            path = tempdir())
#'
#' ## Write investigation file to a temporary directory.
#' writeInvestigationFile(isaObject = isaObject1,
#'                        path = tempdir())
#'
#' ## Write study file to a temporary directory.
#' writeStudyFiles(isaObject = isaObject1,
#'                 studyFilenames = "s_study1.txt",
#'                 path = tempdir())
#'
#' ## Write assay file to a temporary directory.
#' writeAssayFiles(isaObject = isaObject1,
#'                 assayFilenames = "a_study1.txt",
#'                 path = tempdir())
#'
#' @importFrom utils write.table
#' @export
writeISAtab <- function(isaObject,
                        path = getwd()) {
  writeInvestigationFile(isaObject = isaObject, path = path)
  writeStudyFiles(isaObject = isaObject, path = path)
  writeAssayFiles(isaObject = isaObject, path = path)
}

#' @rdname writeISAtab
#' @export
writeInvestigationFile <- function(isaObject,
                                   path = getwd()) {
  iSections <- c("oSR", "invest", "iPubs", "iContacts")
  sSections <- c("study", "sDD", "sPubs", "sFacts", "sAssays",
                 "sProts", "sContacts")
  ## Construct full output file name.
  outFile <- file.path(path, iFileName(isaObject))
  ## Create an empty output file.
  ## This requires that the file directory to exists.
  file.create(outFile)
  ## Open the file.
  openFile <- file(outFile, open = "w")
  for (section in iSections) {
    ## Get section content.
    sectionContent <- t(do.call(what = section, args = list(x = isaObject)))
    ## Add content to output file.
    writeSection(section = section,
                 sectionContent = sectionContent,
                 outFile = openFile)
  }
  studies <- names(getStudyFileNames(isaObject))
  ## Study content in investigation file is added per study.
  for (study in studies) {
    for (section in sSections) {
      ## Get section content.
      sectionContent <- t(do.call(what = section, args = list(x = isaObject))[[study]])
      ## Add content to output file.
      writeSection(section = section,
                   sectionContent = sectionContent,
                   outFile = openFile)
    }
  }
  close(openFile)
}

#' @param studyFilenames A character vector indicating the study files that
#' should be written. Default all study files in isaObject are written.
#'
#' @rdname writeISAtab
#' @export
writeStudyFiles <- function(isaObject,
                            studyFilenames = getStudyFileNames(isaObject),
                            path = getwd()){
  studyFiles <- sFiles(isaObject)
  missStudyFiles <- studyFilenames[!studyFilenames %in% names(studyFiles)]
  if (length(missStudyFiles) > 0) {
    stop("The following study files are not present in the isaObject:\n",
         paste(missStudyFiles, collapse = ","))
  }
  for (studyFilename in studyFilenames) {
    studyContent <- studyFiles[[studyFilename]]
    ## Construct full output file name.
    outFile <- file.path(path, studyFilename)
    ## Create an empty output file.
    file.create(outFile)
    ## Write output to file.
    write.table(studyContent,
                file = outFile,
                row.names = FALSE,
                col.names = TRUE,
                quote = TRUE,
                sep = "\t",
                na = "\"\"",
                fileEncoding = "UTF8")
  }
}

#' @param assayFilenames A character vector indicating the assay files that
#' should be written. Default all assay files in isaObject are written.
#'
#' @rdname writeISAtab
#' @export
writeAssayFiles <- function(isaObject,
                            assayFilenames = unlist(getAssayFileNames(isaObject)),
                            path = getwd()) {
  assayFiles <- aFiles(isaObject)
  missAssayFiles <- assayFilenames[!assayFilenames %in% names(assayFiles)]
  if (length(missAssayFiles) > 0) {
    stop("The following assay files are not present in the isaObject:\n",
         paste(missAssayFiles, collapse = ","))
  }
  for (assayFilename in assayFilenames) {
    assayContent <- assayFiles[[assayFilename]]
    ## Construct full output file name.
    outFile <- file.path(path, assayFilename)
    ## Create an empty output file.
    file.create(outFile)
    ## Write output to file.
    write.table(assayContent,
                file = outFile,
                row.names = FALSE,
                col.names = TRUE,
                quote = TRUE,
                sep = "\t",
                na = "\"\"",
                fileEncoding = "UTF8")
  }
}

#' Helper function for writing sections
#'
#' Helper function for adding a single section to an output file.
#'
#' @param section A length-one character vector indicating the section name
#' matching a section name in ISASyntax.
#' @param sectionContent A data.frame with the content of the section that is to
#' be written.
#' @param outFile A length-one character vector containing the full path to the
#' output file.
#'
#' @noRd
#' @keywords internal
writeSection <- function(section = "",
                         sectionContent,
                         outFile) {
  ## Write section header.
  cat(paste0(ISASyntax[[section]], "\n"),
      file = outFile,
      append = TRUE)
  ## Write section content line by line to allow for unquoted row names.
  for (i in seq_len(nrow(sectionContent))) {
    ## Write row names - unquoted.
    cat(paste0(rownames(sectionContent)[i], "\t"),
        file = outFile,
        append = TRUE)
    ## Write content - quoted.
    write.table(sectionContent[i, , drop = FALSE],
                file = outFile,
                row.names = FALSE,
                col.names = FALSE,
                quote = TRUE,
                sep = "\t",
                na = "\"\"",
                append = TRUE,
                fileEncoding = "UTF8")
  }
}

