\name{searchPost}
\alias{searchPost}
\title{Searching for Google+ Posts}
\usage{
searchPost(q, ret = "data.frame", language = NULL, maxPages = 1,
  nextToken = NULL, page = 1)
}
\arguments{
  \item{q}{The query string to search.}

  \item{ret}{A string specifying the kind of return value.
  Either a \code{list} of the rerieved items on the page,
  or that list parsed into a \code{data.frame}.}

  \item{language}{A language code. See
  \url{https://developers.google.com/+/api/search#available-languages}.}

  \item{maxPages}{Google fits 20 results on each page. This
  parameter specifies how many pages to retrieve at most.
  By default, only the first page is being retrieved.}

  \item{nextToken,page}{used internally to retrieve
  additional pages of answers from the Google+ API. Users
  won't need to set this argument.}
}
\value{
The function returns a list or a data frame containing all
available data on the posts that met the search criteria.
See \code{Details} for more on its content.
}
\description{
This function uses the Google+ API to search for a text
string in posts. Optionally, search results can be
restricted to a certain language.
}
\details{
The result is either a simple list of items from the page
that can be parsed using \code{\link{parsePost}} or a data
frame with that function allready applied.
}
\examples{
\dontrun{
searchPost("#cats")
}
}

