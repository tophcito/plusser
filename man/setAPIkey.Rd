\name{setAPIkey}
\alias{setAPIkey}
\title{Sets an API key for the Google+ API}
\usage{
setAPIkey(apikey)
}
\arguments{
  \item{apikey}{the API key as a character string.}
}
\value{
Returns \code{TRUE} if the key was stored successfully.
}
\description{
This function sets an API key that is then stored
invisibly, for \code{plusser} to use when accessing the
Google+ API.
}
\examples{
setAPIkey("thisIsInvalid")
}

