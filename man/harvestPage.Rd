\name{harvestPage}
\alias{harvestPage}
\title{Retrieve the posts of a user's G+ page.}
\usage{
harvestPage(user, ret = "data.frame")
}
\arguments{
  \item{user}{The UID of a user.}

  \item{ret}{A string specifying the kind of return value.
  Either a \code{list} of the rerieved items on the page,
  or that list parsed into a \code{data.frame}.}
}
\value{
The function returns a list or a data frame. See
\code{Details} for more on its content.
}
\description{
This function retrieves up to the 100 last posts that a
user put on her page.
}
\details{
The result is either a simple list of items from the page
that can be parsed using \code{\link{parsePost}} or a data
frame with that function allready applied.
}
\examples{
\dontrun{
myPosts.df <- harvestPage("115046504166916768425")
}
}

