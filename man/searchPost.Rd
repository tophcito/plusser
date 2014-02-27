\name{searchPost}
\alias{searchPost}
\title{Searching for Google+ Posts}
\usage{
searchPost(q, ret = "data.frame", language = NULL, results = 1,
  nextToken = NULL, cr = 0)
}
\arguments{
  \item{q}{The query string to search. The string is URL
  encoded automatically.}

  \item{ret}{A string specifying the kind of return value.
  Either a \code{list} of the retrieved items on the page,
  or that list parsed into a \code{data.frame}.}

  \item{language}{A language code. See
  \url{https://developers.google.com/+/api/search#available-languages}.}

  \item{results}{The approximate number of results that
  will be retrieved from Google+.}

  \item{nextToken,cr}{used internally to retrieve
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
frame with that function already applied.

The length of the list or the number of rows of the data
frame are somewhat ambiguous. Specifying the \code{results}
argument will try to get that many results. But there may
be less (because Google could not find more) or more
(because Google is organizing results on pages and it would
be a waste to discard them automatically). If you really
depend on getting not more rows than you expected, use
standard selection (i.e. \code{[}) to trim the results.
}
\examples{
\dontrun{
searchPost("#cats")
}
}
\seealso{
Google+ API documentation:
\url{https://developers.google.com/+/api/latest/activities/search}.
}

