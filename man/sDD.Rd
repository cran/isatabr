% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R, R/setInfo.R
\name{sDD}
\alias{sDD}
\alias{sDD<-}
\alias{sDD,ISA-method}
\alias{sDD<-,ISA-method}
\title{Get and set sDD.}
\usage{
sDD(x)

sDD(x) <- value

\S4method{sDD}{ISA}(x)

\S4method{sDD}{ISA}(x) <- value
}
\arguments{
\item{x}{An object of class \linkS4class{ISA}.}

\item{value}{A list of data.frames containing the study design descriptors.
In each data.frame at least the following columns are present:
'Study Design Type', 'Study Design Type Term Accession Number', 'Study Design Type Term Source REF'.}
}
\value{
A list of data.frames containing the study design descriptors.

The updated object of class \linkS4class{ISA}.
}
\description{
Get and set the list of sDD data.frames in an object of
\linkS4class{ISA}.
}
\examples{
## Read example Atwell data set.
isaObject1 <- readISATab(path = file.path(system.file("extdata/Atwell",
                                          package = "isatabr")))

## Extract list of sDD data.frames.
sDD(isaObject1)

}
