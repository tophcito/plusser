\docType{package}
\name{plusser}
\alias{plusser}
\alias{plusser-package}
\title{plusser}
\description{
The idea of this package is to provide a high level
interface to Google's Google+ social network. As of now,
only very basic functions are available.
}
\details{
The basic workflow is as follows: \enumerate{ \item Set you
API key using \code{\link{setAPIkey}}.  \item Locate the
Google+ user ID of the page you want to look up.
Typically, be searching for that entity on Google+ and
noting the UID.  \item Harvest the page of that entitiy
using \code{\link{harvestPage}}.  \item Use the resulting
data frame as you see fit. }
}

