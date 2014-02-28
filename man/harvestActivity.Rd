\name{harvestActivity}
\alias{harvestActivity}
\title{Retrieve the users that acted on a G+ post}
\usage{
harvestActivity(activity, kind = c("plusoners", "resharers"),
  nextToken = NULL)
}
\arguments{
  \item{activity}{The post ID for which the users should be
  retrieved.}

  \item{kind}{Denoting the kind of person to be retrieved.
  Either \code{plusoners} or \code{resharers}.}

  \item{nextToken}{This is used internally to retrieve
  additional pages of answers from the Google+ API. Users
  won't need to set this argument.}
}
\value{
Returns a (character) vector of Google+ user IDs.
}
\description{
This function retrieves the users that either +1ed or
reshared a post. Google calls this `list by activity`.
}
\examples{
\dontrun{
## User IDs of people that +1ed this post
users.p <- harvestActivity("z131ihvycs30ivrxm04cjbiwjkbqujka0sk0k", "plusoners")
}
}
\seealso{
Google+ API documentation:
\url{https://developers.google.com/+/api/latest/people/listByActivity}.
}

