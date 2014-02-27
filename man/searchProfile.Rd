\name{searchProfile}
\alias{searchProfile}
\title{Searching in Google+ Profiles}
\usage{
searchProfile(q, language = NULL, results = 1, nextToken = NULL, cr = 1)
}
\arguments{
  \item{q}{The query string to search. The string is URL
  encoded automatically.}

  \item{language}{A language code. See
  \url{https://developers.google.com/+/api/search#available-languages}.}

  \item{results}{The approximate number of results that
  will be retrieved from Google+.}

  \item{nextToken,cr}{used internally to retrieve
  additional pages of answers from the Google+ API. Users
  won't need to set this argument.}
}
\value{
a data frame with the user ID and display names of the
profiles that met the search criteria.
}
\description{
This function uses the Google+ API to search for a text
string in profiles. Optionally, profiles can be restricted
to a certain language.
}
\details{
The number of rows of the data frame returned is somewhat
ambigious. Specifying the \code{results} argument will try
to get that many results. But there may be less (because
Google could not find more) or more (because Google is
organizing results on pages and it would be a waste to
discard them automatically). If you really depend on
getting not more rows than you expected, use standard
selection (i.e. \code{[}) to trim the results.
}
\examples{
\dontrun{
searchProfile("cats")
}
}

