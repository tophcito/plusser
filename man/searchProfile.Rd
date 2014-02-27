\name{searchProfile}
\alias{searchProfile}
\title{Searching in Google+ Profiles}
\usage{
searchProfile(q, language = "en", maxPages = 1, nextToken = NULL,
  page = 1)
}
\arguments{
  \item{q}{The query string to search.}

  \item{language}{A language code. See
  \url{https://developers.google.com/+/api/search#available-languages}.}

  \item{maxPages}{Google fits 50 results on each page. This
  parameter specifies how many pages to retrieve at most.
  By default, only the first page is being retrieved.}

  \item{nextToken,page}{used internally to retrieve
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
\examples{
\dontrun{
searchProfile("cats")
}
}

