\name{harvestActivity}
\alias{harvestActivity}
\title{Retrieve the users that acted on a G+ post}
\usage{
harvestActivity(activity, kind = c("plusoners", "resharers"),
  nextToken = NULL)
}
\arguments{
  \item{activity}{the post ID for which the users should be
  retrieved.}

  \item{kind}{denoting the kind of person to be retrieved.
  Either \code{plusoners} or \code{resharers}.}

  \item{nextToken}{used internally to retrieve additional
  pages of answers from the Google+ API. Users won't need
  to set this argument.}
}
\value{
Returns a (character) vector of Google+ user IDs.
}
\description{
This function retrieves the users that either +1ed or
reshared a post.
}

