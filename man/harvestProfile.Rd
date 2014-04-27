\name{harvestProfile}
\alias{harvestProfile}
\title{Retrieve the profile of Google+ users}
\usage{
harvestProfile(id, parseFun = parseProfile)
}
\arguments{
  \item{id}{A character vector of the Google+ user ID(s).}

  \item{parseFun}{the function used to parse the results.
  If \code{NULL} the raw list of results is returned.}
}
\value{
The function returns either a raw list or a parsed version.
See \code{Details}.
}
\description{
This function retrieves the profile of one or more Google+
user(s). Google calls this `get people`. The results are
either returned as a raw list with one element per profile
or parsed using a parsing function, either the prepackaged
one \code{\link{parseProfile}} or a user-supplied one.
}
\details{
When using your own parsing function, be sure that it takes
a single element from the returned list and returns either
a vector of values or a single row data frame.
}
\examples{
\dontrun{
gProfile <- harvestProfile("+google")
}
}
\seealso{
Google+ API documentation:
\url{https://developers.google.com/+/api/latest/people/get}
}

