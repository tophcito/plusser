\name{harvestPage}
\alias{harvestPage}
\title{Retrieve the posts of a user's G+ page}
\usage{
harvestPage(user, parseFun = parsePost, results = 1, nextToken = NULL,
  cr = 1)
}
\arguments{
  \item{user}{A user identification string: either user ID
  or +Name.}

  \item{parseFun}{A function for parsing the results, e.g.
  the supplied \code{\link{parsePost}} function. If set to
  \code{NULL}, then the raw list of the retrieved posts is
  being returned. Defaults to \code{parsePost}.}

  \item{results}{The approximate number of results that
  will be retrieved from Google+.}

  \item{nextToken,cr}{used internally to retrieve
  additional pages of answers from the Google+ API. Users
  won't need to set these arguments.}
}
\value{
The function returns either the raw list of retrieved posts
or whatever the supplied parsing function does with the
retrieved list.
}
\description{
This function retrieves the most recent posts that a user
put on her page. Google calls this `listing activities`.
}
\details{
The result is either a simple list of items from the page
that can be parsed using e.g. \code{\link{parsePost}} or a
data frame with that (or another user-supplied) function
already applied.

When writing your own parsing functions, make sure that the
function takes a single list item from the raw list as its
argument and returns a vector of values or a one-row data
frame. The return values of the function are then fed into
\code{plyr}'s \code{ldply} to turn it into a data frame.
See \code{\link{parsePost}} for an example.

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
gPosts.df <- harvestPage("+google", results=200)
}
}
\seealso{
Google+ API documentation:
\url{https://developers.google.com/+/api/latest/activities/list}.
}

