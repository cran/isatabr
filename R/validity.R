#' Check the validity of an object of class ISA.
#'
#' The \code{validISAObject} function checks whether an object of class
#' \linkS4class{ISA} is a valid object. An object of the \linkS4class{ISA} is
#' considered valid when:
#' * There is only one investigation file, which name starts with **i_**
#' and ends with **.txt**, present in the folder containing the ISA-Tab files.
#' * All sections in the investigation file contain at least the columns
#' specified for that specific section at
#' https://isa-specs.readthedocs.io/en/latest/isatab.html
#' * The study and assay files contain at least the columns specified at
#' https://isa-specs.readthedocs.io/en/latest/isatab.html
#'
#' @param object An object of class \linkS4class{ISA}.
#'
#' @return TRUE or an error message.
#'
#' @seealso \linkS4class{ISA}
#'
#' @examples
#' ## Read example Atwell data set.
#' isaObject1 <- readISATab(path = file.path(system.file("extdata/Atwell",
#'                                           package = "isatabr")))
#'
#' ## Check validity of the object.
#' validISAObject(isaObject1)
#'
#' @export
validISAObject <- function(object) {
  ## Check that path points to an existing folder.
  objPath <- isaPath(object)
  if (!file.exists(objPath)) {
    stop(objPath, " is not an existing folder on this system.\n")
  } else {
    objPath <- normalizePath(objPath)
  }
  ## Check number of investigation files - should be 1.
  objiFileName <- iFileName(object)
  noIFilenames <- length(objiFileName)
  if (noIFilenames == 0) {
    stop("Did not find any investigation file at folder ", objPath, ".\n")
  } else if (noIFilenames > 1) {
    stop("Found too many possible investigation files: ",
         paste(objiFileName, collapse = ", "), "\n")
  }
  ## Check structure of investigation file name.
  if (!grepl(pattern = paste0("^",
                              ISASyntax$iPrefix,
                              ".*[a-zA-Z0-9_-]",
                              "(\\.txt)$"),
             x = objiFileName,
             perl = TRUE)) {
    stop(paste0("The investigation file: \"",
                objiFileName,
                "\" for the \"",
                ISASyntax$iFileName,
                "\" slot does not match the requirements (start with ",
                "\"i_\" and end with \".txt\").\n"))
  }
  ## Check column names in ontology source reference.
  checkMinCols(object, "oSR")
  ## Check column names in investigation info.
  checkMinCols(object, "invest")
  ## Check column names in investigation publications info.
  checkMinCols(object, "iPubs")
  ## Check column names in investigation contacts info.
  checkMinCols(object, "iContacts")
  ## Check columns names for study info.
  checkMinColsStudy(object, "study")
  ## Check columns names for study design descriptors.
  checkMinColsStudy(object, "sDD")
  ## Check columns names for study publications.
  checkMinColsStudy(object, "sPubs")
  ## Check columns names for study factors.
  checkMinColsStudy(object, "sFacts")
  ## Check columns names for study assays.
  checkMinColsStudy(object, "sAssays")
  ## Check columns names for study protocols.
  checkMinColsStudy(object, "sProts")
  ## Check columns names for study protocols.
  checkMinColsStudy(object, "sContacts")
  ## Check columns names for study files.
  checkMinColsStudy(object, "sFiles")
  ## Check columns names for assay files.
  checkMinColsStudy(object, "aFiles")

}
setValidity(Class = "ISA",
            method = validISAObject)
