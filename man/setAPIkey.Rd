\name{setAPIkey}
\alias{setAPIkey}
\title{Sets an API key for the Google+ API}
\usage{
setAPIkey(apikey)
}
\arguments{
  \item{apikey}{The API key as a character string.}
}
\value{
Returns \code{TRUE} if the key was stored successfully.
}
\description{
This function sets an API key that is then stored invisibly
for \code{plusser} to use when accessing the Google+ API. A
warning is issued if URL escaping the api key alters it, as
Google should provide you with a HTML-safe API key in the
first place.
}
\examples{
setAPIkey("thisIsInvalid")
}

