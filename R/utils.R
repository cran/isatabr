### start ISASyntax list ----
ISASyntax <- list(
  ## ISA-Tab File Path
  path = "path",
  ## ISA-Tab File Prefixes
  iPrefix = "i_",
  sPrefix = "s_",
  aPrefix = "a_",
  ## ISA-Tab File Name
  iFileName = "Investigation File Name",
  sFileName = "Study File Name",
  aFileName = "Study Assay File Name",
  ## ISA-Tab: Files
  sFiles = "Study Files",
  aFiles = "Assay Files",
  ## ISA-Tab: Investigation File Section Headings (in order of appearance)
  oSR       = "ONTOLOGY SOURCE REFERENCE",
  invest    = "INVESTIGATION",
  iPubs     = "INVESTIGATION PUBLICATIONS",
  iContacts = "INVESTIGATION CONTACTS",
  study     = "STUDY",
  sDD       = "STUDY DESIGN DESCRIPTORS",
  sPubs     = "STUDY PUBLICATIONS",
  sFacts    = "STUDY FACTORS",
  sAssays   = "STUDY ASSAYS",
  sProts    = "STUDY PROTOCOLS",
  sContacts = "STUDY CONTACTS",
  ## ISA-Tab: Data Frame Column Names
  iidentifier    = "Investigation Identifier",
  sidentifier    = "Study Identifier",
  assayName      = "Assay Name",
  sAssayTechType = "Study Assay Technology Type",
  sAssayMeasType = "Study Assay Measurement Type",
  ## Raw Data File Column Names - depend on technology type.
  rawDataFile    = c(base       = "Raw Data File",
                     microarray = "Array Data File",
                     ms         = "Raw Spectral Data File",
                     "Free Induction Decay Data File"),
  ## Derived Data File Column Names - depend on technology type.
  derivedDataFile = c(base       ="Derived Data File",
                      microarray = "Derived Array Data File",
                      ms         = "Derived Spectral Data File"),
  ## ISA-Tab: Data Frame Column Names grep pattern
  dataFile   = "Data File",
  sampleName = "Sample Name",
  fctrValue  = "Factor Value",
  ## Column names study info.
  sTitle = "Study Title",
  sDescription = "Study Description",
  sPersonLast = "Study Person Last Name",
  sPersonFirst = "Study Person First Name",
  sPersonMid = "Study Person Mid Initials",
  sPersonAff = "Study Person Affiliation",
  sPersonRoles = "Study Person Roles",
  arrayDataFile = "Array Data File",
  rawSpecDataFile = "Raw Spectral Data File"
)
### end ISASyntax list ----

### Minimum required columns for data.frames.
oSRCols <- c("Term Source Name",
             "Term Source File",
             "Term Source Version",
             "Term Source Description")
investCols <- c("Investigation Identifier",
                "Investigation Title",
                "Investigation Description",
                "Investigation Submission Date",
                "Investigation Public Release Date")
iPubsCols <- c("Investigation PubMed ID",
               "Investigation Publication DOI",
               "Investigation Publication Author List",
               "Investigation Publication Title",
               "Investigation Publication Status",
               "Investigation Publication Status Term Accession Number",
               "Investigation Publication Status Term Source REF")
iContactsCols <- c("Investigation Person Last Name",
                   "Investigation Person First Name",
                   "Investigation Person Mid Initials",
                   "Investigation Person Email",
                   "Investigation Person Phone",
                   "Investigation Person Fax",
                   "Investigation Person Address",
                   "Investigation Person Affiliation",
                   "Investigation Person Roles",
                   "Investigation Person Roles Term Accession Number",
                   "Investigation Person Roles Term Source REF")
studyCols <- c("Study Identifier",
               "Study Title",
               "Study Description",
               "Study Submission Date",
               "Study Public Release Date",
               "Study File Name")
sDDCols <- c("Study Design Type",
             "Study Design Type Term Accession Number",
             "Study Design Type Term Source REF")
sPubsCols <- c("Study PubMed ID",
               "Study Publication DOI",
               "Study Publication Author List",
               "Study Publication Title",
               "Study Publication Status",
               "Study Publication Status Term Accession Number",
               "Study Publication Status Term Source REF")
sFactsCols <- c("Study Factor Name",
                "Study Factor Type",
                "Study Factor Type Term Accession Number",
                "Study Factor Type Term Source REF")
sAssaysCols <- c("Study Assay Measurement Type",
                 "Study Assay Measurement Type Term Accession Number",
                 "Study Assay Measurement Type Term Source REF",
                 "Study Assay Technology Type",
                 "Study Assay Technology Type Term Accession Number",
                 "Study Assay Technology Type Term Source REF",
                 "Study Assay Technology Platform",
                 "Study Assay File Name")
sProtsCols <- c("Study Protocol Name",
                "Study Protocol Type",
                "Study Protocol Type Term Accession Number",
                "Study Protocol Type Term Source REF",
                "Study Protocol Description",
                "Study Protocol URI",
                "Study Protocol Version",
                "Study Protocol Parameters Name",
                "Study Protocol Parameters Name Term Accession Number",
                "Study Protocol Parameters Name Term Source REF",
                "Study Protocol Components Name",
                "Study Protocol Components Type",
                "Study Protocol Components Type Term Accession Number",
                "Study Protocol Components Type Term Source REF")
sContactsCols <- c("Study Person Last Name",
                   "Study Person First Name",
                   "Study Person Mid Initials",
                   "Study Person Email",
                   "Study Person Phone",
                   "Study Person Fax",
                   "Study Person Address",
                   "Study Person Affiliation",
                   "Study Person Roles",
                   "Study Person Roles Term Accession Number",
                   "Study Person Roles Term Source REF")
sFilesCols <- c("Source Name",
                "Term Source REF",
                "Sample Name")
aFilesCols <- c()

### start technologyTypes list ----
## Technology types is a free field.
## Some technologies may have different/additional defined columns.
## Those technologies are specified here
technologyTypes <- list(
  microarray = "DNA microarray",
  gelelecto  = "Gel electrophoresis",
  fc         = "flow cytometry",
  ms         = "mass spectrometry",
  NMR        = "NMR spectroscopy",
  seq        = "nucleotide sequencing"
)
### end technologyTypes list ----

### start helper functions ----

checkCharacter <- function(...) {
  args <- list(...)
  if (!all(sapply(args, is.character))) {
    stop("The provided arguments must be of class character.")
  }
}

#' Check required columns
#'
#' Helper function for checking that the minimum required columns in a
#' data.frame are present.
#'
#' @importFrom utils hasName
#' @noRd
#' @keywords internal
checkMinCols <- function(isaObject,
                         section) {
  ## Get the data.frame from the ISA object.
  df <- do.call(section, list(isaObject))
  ## Get the required columns.
  reqCols <- get(paste0(section, "Cols"))
  ## Check for missing columns.
  missCols <- reqCols[!hasName(df, reqCols)]
  if (length(missCols) > 0) {
    stop("Not all minimal required columns are present for ", section, ". ",
         "The following columns are missing: ",
         paste(missCols, collapse = ", "), "\n")
  }
}

#' Check required columns per study
#'
#' Helper function for checking that the minimum required columns in a list
#' of data.frames, one per study, are present.
#'
#' @importFrom utils hasName
#' @noRd
#' @keywords internal
checkMinColsStudy <- function(isaObject,
                              section) {
  ## Get the data.frame from the ISA object.
  lst <- do.call(section, list(isaObject))
  ## Get the required columns.
  reqCols <- get(paste0(section, "Cols"))
  ## Check for missing columns.
  for (study in names(lst)) {
    missCols <- reqCols[!hasName(lst[[study]], reqCols)]
    if (length(missCols) > 0) {
      stop("Not all minimal required columns are present for study ", study,
           " in ", section, ". ",
           "The following columns are missing: ",
           paste(missCols, collapse = ", "), "\n")
    }
  }
}

#' Helper function for creating data frames for slots in ISA object.
#'
#' @noRd
#' @keywords internal
createISASlotDataFrame <- function(file,
                                   startRow,
                                   endRow) {
  tempdf <- as.data.frame(t(file[(startRow + 1):(endRow - 1), ]),
                          stringsAsFactors = FALSE)
  colnames(tempdf) <- tempdf[1, ]
  tempdf <- tempdf[-c(1), ]
  rownames(tempdf) <- NULL
  if (any(rowSums(!is.na(tempdf)) == 0)) {
    tempdf <- tempdf[-c(which(rowSums(!is.na(tempdf)) == 0)), ]
  }
  return(tempdf)
}
### end helper functions
