#' Retrieve Assay Tables from an ISA object.
#'
#' Retrieve from an object of the \code{\link{ISA-class}} the Assay Tables.
#'
#' @inheritParams writeISAtab
#'
#' @return A list of lists of objects of class \code{\link{assayTab}}, where
#' each list element, named by the Study Identifier, contains a list of
#' objects of class \code{\link{assayTab}}.
#'
#' @examples
#' ## Read example Atwell data set.
#' isaObject1 <- readISATab(path = file.path(system.file("extdata/Atwell",
#'                                           package = "isatabr")))
#'
#' ## Get assay tabs for isaObject1.
#' aTabObjects <- getAssayTabs(isaObject1)
#'
#' @export
getAssayTabs <- function(isaObject) {
  ## Get info from isaObject.
  studyFileNames <- names(sFiles(isaObject))
  assayInfo <- sAssays(isaObject)
  assayFiles <- aFiles(isaObject)
  assayTabs <- lapply(X = seq_along(studyFileNames), FUN = function(i) {
    studyId <- names(assayInfo)[i]
    studyAssay <- assayInfo[[studyId]]
    assayTabsStudy <- lapply(X = 1:nrow(studyAssay), FUN = function(j) {
      ## Class is dependent of technology type.
      ## Returns empty character for 'non-existing' technology.
      assayTechType <- studyAssay[j, ISASyntax$sAssayTechType]
      assayTechName <- names(technologyTypes)[technologyTypes == assayTechType]
      assayClass <- if (isTRUE(nzchar(assayTechName)))
        paste0(assayTechName, "AssayTab") else "assayTab"
      assayMeasType <- studyAssay[j, ISASyntax$sAssayMeasType]
      new(assayClass,
          path = isaPath(isaObject),
          sFilename = studyFileNames[i],
          sIdentifier = studyId,
          aFilename = studyAssay[j, ISASyntax$aFileName],
          aFile = assayFiles[[studyAssay[j, ISASyntax$aFileName]]],
          aTechType = assayTechType,
          aMeasType = assayMeasType
      )
    })
    names(assayTabsStudy) <- studyAssay[[ISASyntax$aFileName]]
    return(assayTabsStudy)
  })
  names(assayTabs) <- studyFileNames
  return(assayTabs)
}

#' Process assay tab data
#'
#' Process data from assay tab files
#'
#' @param isaObject An object of the \linkS4class{ISA}.
#' @param aTabObject An object of the \linkS4class{assayTab}.
#' @param type A character string indicating which data files should be
#' processed, either "raw" for raw data files, or "derived" for derived data
#' files. The file names are taken from the corresponding column in the
#' \code{aTabObject}.
#'
#' @examples
#' ### Atwell data.
#'
#' ## Read example Atwell data set.
#' isaObject1 <- readISATab(path = file.path(system.file("extdata/Atwell",
#'                                           package = "isatabr")))
#'
#' ## Get assay tabs for isaObject1.
#' aTabObjects <- getAssayTabs(isaObject1)
#'
#' ## Process assay data.
#' isaDat <- processAssay(isaObject = isaObject1,
#'                       aTabObject = aTabObjects$s_study1.txt$a_study1.txt,
#'                        type = "derived")
#'
#' ## Display first rows and columns.
#' head(isaDat[, 1:10])
#'
#' \donttest{
#' ### faahKO data. - This requires the xcms package to be installed.
#' ## Read ISA-Tab files for faahKO.
#' if (requireNamespace("xcms")) {
#'   isaObject3 <- readISATab(path = file.path(system.file("extdata/faahKO",
#'                                             package = "isatabr")))
#'
#'   ## Get assay tabs for isaObject3.
#'   aTabObjects3 <- getAssayTabs(isaObject3)
#'
#'   ## Process assay data.
#'   isaDat3 <-
#'     processAssay(isaObject = isaObject3,
#'                  aTabObject = aTabObjects3$s_Proteomic_profiling_of_yeast.txt$a_metabolite.txt,
#'                  type = "raw")
#'
#'   ## Display output.
#'   isaDat3
#'   }
#' }
#'
#' @docType methods
#' @rdname processAssay-methods
#' @exportMethod processAssay
setGeneric("processAssay",
           function(isaObject,
                    aTabObject,
                    type = c("raw", "derived")) standardGeneric("processAssay"))


#' @rdname processAssay-methods
#' @aliases processAssay,ISA,assayTab-method
setMethod(f = "processAssay",
          signature = c(isaObject = "ISA",
                        aTabObject = "assayTab",
                        type = "character"),
          definition = function(isaObject, aTabObject, type) {
            type <- match.arg(type)
            assayDat <- slot(aTabObject, "aFile")
            ## Construct column name containing files.
            fileCol <- ISASyntax[[paste0(type, "DataFile")]]["base"]
            ## Get file names.
            datFiles <- file.path(normalizePath(slot(aTabObject, "path"),
                                                winslash = .Platform$file.sep),
                                  unique(assayDat[[fileCol]]))
            ## Check that files exist.
            missFiles <- datFiles[!file.exists(datFiles)]
            if (length(missFiles) > 0) {
              stop("The following files are not found:\n",
                   paste(missFiles, collapse = ", "))
            }
            ## Read file contents.
            assayContLst <- lapply(X = datFiles, FUN = function(datFile) {
              tempdf <- read.delim(datFile,
                                   header = TRUE,
                                   check.names = FALSE,
                                   blank.lines.skip = TRUE,
                                   stringsAsFactors = FALSE,
                                   fileEncoding = "UTF8")
              ## Remove empty rows.
              tempdf <- tempdf[apply(tempdf, 1, function(x) any(nzchar(x))), ]
              ## Remove empty columns.
              # Save and re-add colnames to assure duplicate names are not removed.
              colNames <- colnames(tempdf)
              tempdf <- tempdf[, nzchar(colnames(tempdf))]
              colnames(tempdf) <- colNames[nzchar(colnames(tempdf))]
              return(tempdf)
            })
            assayCont <- do.call(rbind, assayContLst)
            return(assayCont)
          }
)

#' Process assay tab data for mass spectrometry
#'
#' Process data from assay tab files with technology type mass spectrometry
#' (ms). Processing those files requires the xcms package to be installed.
#'
#' @param isaObject An object of the \linkS4class{ISA}.
#' @param aTabObject An object of the \linkS4class{msAssayTab}.
#'
#' @rdname processAssay-methods
#' @aliases processAssay,ISA,msAssayTab-method
setMethod(f = "processAssay",
          signature = c(isaObject = "ISA", aTabObject = "msAssayTab", type = "character"),
          definition = function(isaObject, aTabObject, type) {
            if (requireNamespace("xcms", quietly = TRUE)) {
              type <- match.arg(type)
              assayDat <- slot(aTabObject, "aFile")
              ## Construct column name containing files.
              fileCol <- ISASyntax[[paste0(type, "DataFile")]]["ms"]
              spectralDatFiles <-
                file.path(normalizePath(slot(aTabObject, "path"),
                                        winslash = .Platform$file.sep),
                          unique(assayDat[[fileCol]]))
              ## Check that files exist.
              missFiles <- spectralDatFiles[!file.exists(spectralDatFiles)]
              if (length(missFiles) > 0) {
                stop("The following files are not found:\n",
                     paste(missFiles, collapse = ", "))
              }
              ## Construct vector of factors in assay.
              isaFactors <- getFactors(isaObject = isaObject)
              assayFactors <- names(isaFactors[[slot(aTabObject, "sIdentifier")]])
              if (length(assayFactors) > 0) {
                ## Construct data.frame with assay factors only.
                sClass <- assayDat[, assayFactors, drop = FALSE]
                for (colName in colnames(sClass)) {
                  if (!is.factor(sClass[[colName]])) {
                    ## Convert to factor if not a factor already.
                    sClass[[colName]] <- as.factor(sClass[[colName]])
                  }
                }
                xset <- xcms::xcmsSet(files = spectralDatFiles,
                                      sclass = sClass)
              } else {
                xset = try(xcms::xcmsSet(files = spectralDatFiles,
                                         phenoData = assayDat))
              }
              return(xset)
            } else {
              stop("For reading mass spectrometry data the xcms package ",
                   "should be installed.\n")
            }
          }
)

#' Process assay tab data for DNA microarray
#'
#' Process data from assay tab files with technology type DNA microarray
#' (ms). Processing those files requires the Biobase and affy packages to be
#' installed.
#'
#' @param isaObject An object of the \linkS4class{ISA}.
#' @param aTabObject An object of the \linkS4class{microarrayAssayTab}.
#'
#' @rdname processAssay-methods
#' @aliases processAssay,ISA,msAssayTab-method
setMethod(f = "processAssay",
          signature = c(isaObject = "ISA", aTabObject = "microarrayAssayTab", type = "character"),
          definition = function(isaObject, aTabObject, type) {
            if (requireNamespace("affy", quietly = TRUE)) {
              type <- match.arg(type)
              assayDat <- slot(aTabObject, "aFile")
              ## Construct column name containing files.
              fileCol <- ISASyntax[[paste0(type, "DataFile")]]["microarray"]
              ## Get microarray files for assay.
              microarrayDatFiles <- unique(assayDat[[fileCol]])
              microarrayDatFilesFull <-
                file.path(normalizePath(slot(aTabObject, "path"),
                                        winslash = .Platform$file.sep),
                          microarrayDatFiles)
              ## Check that files exist.
              missFiles <- microarrayDatFilesFull[!file.exists(microarrayDatFilesFull)]
              if (length(missFiles) > 0) {
                stop("The following files are not found:\n",
                     paste(missFiles, collapse = ", "))
              }
              ## Construct meta data.
              aTabMIAME <- constructMIAMEMetadata(isaObject = isaObject,
                                                  aTabObject = aTabObject)
              ## Get expression set.
              ## justRMA only reads files from working directory.
              ## So change to that and reset when exiting function.
              wd <- getwd()
              on.exit(setwd(wd), add = TRUE)
              setwd(slot(aTabObject, "path"))
              xset <- affy::justRMA(filenames = microarrayDatFiles,
                                    #phenoData = assayDat,
                                    description = aTabMIAME)
              return(xset)
            } else {
              stop("For reading DNA microarray data the affy package ",
                   "should be installed.\n")
            }
          }
)

#' Helper function for construction MIAME meta data
#'
#' Helper function for construction of MIAME meta data for DNA microarray
#' assay tabs objects. MIAME is a class in the Biobase package for storing
#' MicroArray Experiment Information.
#'
#' @return An object of class \code{MIAME}.
#'
#' @noRd
#' @keywords internal
#' @importFrom utils person
constructMIAMEMetadata <- function(isaObject,
                                   aTabObject) {
  if (requireNamespace("Biobase")) {
    if (is(aTabObject, "microarrayAssayTab")) {
      assayDat <- slot(aTabObject, "aFile")
      sIdentifier <- names(slot(aTabObject, "sFilename"))
      sInfo <- slot(isaObject, "study")[[sIdentifier]]
      sContacts <- isaObject@sContacts[[sIdentifier]]
      ## Get corresponding author details.
      sCorr <- sContacts[sContacts[[ISASyntax$sPersonRoles]] == "corresponding author", ]
      sCorrPers <- as.character(person(given = paste(sCorr[[ISASyntax$sPersonFirst]],
                                                     sCorr[[ISASyntax$sPersonMid]]),
                                       family = sCorr[[ISASyntax$sPersonLast]]))
      aTabMIAME <- Biobase::MIAME(name = sIdentifier,
                                  lab = sCorr[[ISASyntax$sPersonAff]],
                                  contact = sCorrPers,
                                  title = sInfo[[ISASyntax$sTitle]],
                                  abstract = sInfo[[ISASyntax$sDescription]],
                                  samples = as.list(assayDat["Sample Name"])
      )
      return(aTabMIAME)
    } else {
      stop("MIAME meta data can only be constructed for assays of type ",
           "DNA microarray.\n")
    }
  }
  stop("For constructing MIAME meta data the Biobase package ",
       "should be installed.\n")
}

