#' Read an ISA-Tab dataset into an R object.
#'
#' Reads an ISA-Tab dataset, given as a zip file or as a set of files in a
#' specific folder, and builds an object of the \linkS4class{ISA} class.
#'
#' @param path A character vector with the name of the directory in which the
#' ISA-Tab files are located (if the parameter zipfile is not provided or if it
#' is equal to \code{NULL}), or the name of the directory where the zip archive
#' containing the ISA-Tab files is located (if the parameter zipfile is not
#' \code{NULL}). The default value is the current working directory.
#' @param zipfile A character vector with the name of the zip archive containing
#' ISA-Tab files themselves (without a directory name in the zip archive). The
#' default value is \code{NULL} (specifying that the ISA-Tab files have not
#' been archived in one zipped file).
#' @param verbose Should the messages for the different reading steps be shown?
#'
#' @return An object of the \linkS4class{ISA} class.
#'
#' @examples
#' ## Read example Atwell data set.
#' isaObject1 <- readISATab(path = file.path(system.file("extdata/Atwell",
#'                                           package = "isatabr")))
#'
#' ## Read example zipped Atwell data set.
#' isazip <- "Atwell.zip"
#' isaObject2 <- readISATab(path = file.path(system.file("extdata",
#'                                                       package = "isatabr")),
#'                          zipfile = isazip,
#'                          verbose = TRUE)
#'
#' @export
readISATab <- function(path = getwd(),
                       zipfile = NULL,
                       verbose = FALSE) {
  checkCharacter(path)
  ## Run file.path to assure trailing / are removed/added when needed.
  ## This is required to assure specifying path with/without trailing /
  ## works on all operating systems.
  path <- file.path(path)
  if (!file.exists(path)) {
    stop(path, " is not an existing folder on this system.\n")
  }
  if (!is.null(zipfile)) {
    checkCharacter(zipfile)
    readZippedISATabFiles(path = path, zipfile = zipfile, verbose = verbose)
  } else {
    readISATabFiles(path = path, verbose = verbose)
  }
}

#'  Helper function for reading zipped isatab files.
#'
#' This function only works if the zip file does not contain a directory name
#' (but the ISA-Tab files themselves)
#'
#' @noRd
#' @keywords internal
readZippedISATabFiles <- function(path = getwd(),
                                  zipfile,
                                  verbose = FALSE) {
  ## Create temporary directory for unzipping.
  tmpdir <- normalizePath(tempdir())
  if (verbose) {
    message("Unzipping file in temporary directory: ", tmpdir, "")
  }
  ## Unzip file.
  unzippedISATabFiles <-
    utils::unzip(zipfile = normalizePath(file.path(path, zipfile)),
                 exdir = tmpdir)
  if (verbose) {
    message("Unzipped files: ",
            paste(basename(unzippedISATabFiles), collapse = ", "))
  }
  ## Read from temp directory.
  isaObj <- readISATabFiles(path = tmpdir, verbose = verbose)
  ## Clean up and remove temp directory.
  unlink(tmpdir)
  return(isaObj)
}

#' Helper function for reading isatab files from a specified directory.
#'
#' This function only works if the zip file does not contain a directory name
#' (but the ISA-Tab files themselves)
#'
#' @noRd
#' @keywords internal
readISATabFiles <- function(path = getwd(),
                            verbose = FALSE) {
  if (verbose) {
    message("Converting ISA-Tab dataset at ",
            normalizePath(path),
            " into an R object...")
  }
  isaobject <- new(Class = "ISA", path = normalizePath(path))
  if (verbose) {
    message("... done.")
  }
  return(isaobject)
}
