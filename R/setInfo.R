### Get and set information for an ISA-class object

### Path.

#' @rdname isaPath
setMethod("isaPath", "ISA", function(x) x@path)

#' @rdname isaPath
setMethod("isaPath<-", "ISA", function(x, value) {
  ## nomalizePath is required to protect against system dependent paths,
  ## e.g. extra or missing /.
  x@path <- normalizePath(value)
  validISAObject(x)
  return(x)
})

### iFileName.

#' @rdname iFileName
setMethod("iFileName", "ISA", function(x) x@iFileName)

#' @rdname iFileName
setMethod("iFileName<-", "ISA", function(x, value) {
  x@iFileName <- value
  validISAObject(x)
  return(x)
})

### oSR

#' @rdname oSR
setMethod("oSR", "ISA", function(x) x@oSR)

#' @rdname oSR
setMethod("oSR<-", "ISA", function(x, value) {
  x@oSR <- value
  validISAObject(x)
  return(x)
})

### invest

#' @rdname invest
setMethod("invest", "ISA", function(x) x@invest)

#' @rdname invest
setMethod("invest<-", "ISA", function(x, value) {
  x@invest <- value
  validISAObject(x)
  return(x)
})


### iPubs

#' @rdname iPubs
setMethod("iPubs", "ISA", function(x) x@iPubs)

#' @rdname iPubs
setMethod("iPubs<-", "ISA", function(x, value) {
  x@iPubs <- value
  validISAObject(x)
  return(x)
})


### iContacts

#' @rdname iContacts
setMethod("iContacts", "ISA", function(x) x@iContacts)

#' @rdname iContacts
setMethod("iContacts<-", "ISA", function(x, value) {
  x@iContacts <- value
  validISAObject(x)
  return(x)
})

### study

#' @rdname study
setMethod("study", "ISA", function(x) x@study)

#' @rdname study
setMethod("study<-", "ISA", function(x, value) {
  x@study <- value
  validISAObject(x)
  return(x)
})

### sDD

#' @rdname sDD
setMethod("sDD", "ISA", function(x) x@sDD)

#' @rdname sDD
setMethod("sDD<-", "ISA", function(x, value) {
  x@sDD <- value
  validISAObject(x)
  return(x)
})

### sPubs

#' @rdname sPubs
setMethod("sPubs", "ISA", function(x) x@sPubs)

#' @rdname sPubs
setMethod("sPubs<-", "ISA", function(x, value) {
  x@sPubs <- value
  validISAObject(x)
  return(x)
})

### sFacts

#' @rdname sFacts
setMethod("sFacts", "ISA", function(x) x@sFacts)

#' @rdname sFacts
setMethod("sFacts<-", "ISA", function(x, value) {
  x@sFacts <- value
  validISAObject(x)
  return(x)
})

### sAssays

#' @rdname sAssays
setMethod("sAssays", "ISA", function(x) x@sAssays)

#' @rdname sAssays
setMethod("sAssays<-", "ISA", function(x, value) {
  x@sAssays <- value
  validISAObject(x)
  return(x)
})

### sProts

#' @rdname sProts
setMethod("sProts", "ISA", function(x) x@sProts)

#' @rdname sProts
setMethod("sProts<-", "ISA", function(x, value) {
  x@sProts <- value
  validISAObject(x)
  return(x)
})

### sContacts

#' @rdname sContacts
setMethod("sContacts", "ISA", function(x) x@sContacts)

#' @rdname sContacts
setMethod("sContacts<-", "ISA", function(x, value) {
  x@sContacts <- value
  validISAObject(x)
  return(x)
})

### sFiles

#' @rdname sFiles
setMethod("sFiles", "ISA", function(x) x@sFiles)

#' @rdname sFiles
setMethod("sFiles<-", "ISA", function(x, value) {
  x@sFiles <- value
  validISAObject(x)
  return(x)
})

### aFiles

#' @rdname aFiles
setMethod("aFiles", "ISA", function(x) x@aFiles)

#' @rdname aFiles
setMethod("aFiles<-", "ISA", function(x, value) {
  x@aFiles <- value
  validISAObject(x)
  return(x)
})


### Convenience functions

#' Retrieve the Study Identifier(s) and Study File Name(s) from an ISA object.
#'
#' Retrieve from an object of the \code{\link{ISA-class}} the Study
#' Identifier(s) and Study File Name(s) as contained in the Investigation.
#' To directly access the Study Identifier(s) use the names() function, e.g.
#' \code{names(getStudyFileNames(isaObject))}.
#'
#' @inheritParams writeISAtab
#'
#' @return A named character vector containing the Study File Name(s) and the
#' name(s) representing the Study Identifier(s).
#'
#' @examples
#' ## Read example Atwell data set.
#' isaObject1 <- readISATab(path = file.path(system.file("extdata/Atwell",
#'                                           package = "isatabr")))
#'
#' ## Extract study identifiers and file names.
#' getStudyFileNames(isaObject1)
#'
#' @export
getStudyFileNames <- function(isaObject) {
  sapply(X = study(isaObject), FUN = `[[`, ISASyntax$sFileName)
}

#' Retrieve the Assay File Name(s) per Study from an ISA object.
#'
#' Retrieve from an object of the \code{\link{ISA-class}} the Assay File Name(s)
#' linked to the Study Identifier(s) per Study.
#'
#' @inheritParams writeISAtab
#'
#' @return A named list of character vectors containing the Assay File Name(s)
#' for each Study Identifier. The name of the character vector or names of the
#' list elements represent(s) the Study Identifier(s).
#'
#' #' @examples
#' ## Read example Atwell data set.
#' isaObject1 <- readISATab(path = file.path(system.file("extdata/Atwell",
#'                                           package = "isatabr")))
#'
#' ## Extract assay file names per study.
#' getAssayFileNames(isaObject1)
#'
#' @export
getAssayFileNames <- function(isaObject) {
  lapply(X = sAssays(isaObject), FUN = `[[`, ISASyntax$aFileName)
}


#' Retrieve Factor Values per Study File from an ISA object.
#'
#' Retrieve from an object of the \code{\link{ISA-class}} the Factor Values for
#' each Study File.
#'
#' @inheritParams writeISAtab
#'
#' @return A list of factor lists, where each list element, named by the Study
#' Identifier, contains a list of factors specifying the Factor Values used
#' in a specific Study File linked to the Study Identifier.
#'
#' @noRd
#' @keywords internal
getFactors <- function(isaObject) {
  tmplist <- lapply(X = isaObject@sFiles, FUN = function(df) {
    tmplist <- lapply(X = grep(pattern = ISASyntax$fctrValue,
                               x = colnames(df),
                               value = TRUE),
                      FUN = function(x) {
                        factor(df[[x]])
                      })
    names(tmplist) <- grep(pattern = ISASyntax$fctrValue,
                           x = colnames(df),
                           value = TRUE)
    return(tmplist)
  })
  names(tmplist) <- getStudyFileNames(isaObject)
  return(tmplist)
}
