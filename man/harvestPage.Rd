\name{harvestPage}
\alias{harvestPage}
\title{Retrieve the posts of a user's G+ page}
\usage{
harvestPage(user, ret = "data.frame", results = 1, nextToken = NULL,
  cr = 1)
}
\arguments{
  \item{user}{A user identification string: either user ID
  or +Name.}

  \item{ret}{A string specifying the kind of return value.
  Either a \code{list} of the retrieved items on the page,
  or that list parsed into a \code{data.frame}.}

  \item{results}{The approximate number of results that
  will be retrieved from Google+.}

  \item{nextToken,cr}{used internally to retrieve
  additional pages of answers from the Google+ API. Users
  won't need to set these arguments.}
}
\value{
The function returns a list or a data frame. See
\code{Details} for more on its content.
}
\description{
This function retrieves up to the 100 last posts that a
user put on her page. Google calls this `listing
activities`.
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
myPosts.df <- harvestPage("115046504166916768425")
}
}
\seealso{
Google+ API documentation:
\url{https://developers.google.com/+/api/latest/activities/list}.
}

