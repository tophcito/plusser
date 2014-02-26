\name{parsePost}
\alias{parsePost}
\title{Parsing a Google+ post}
\usage{
parsePost(p)
}
\arguments{
  \item{p}{a raw post as returned from e.g. the
  \code{\link{harvestPage}} function.}
}
\value{
A data frame with the information from the post parsed.
}
\description{
This function turns a Google+ post into a (1 row) data
frame extracting or computing a number of fields. See
\code{Details}.
}
\details{
This function extracts or computes the following fields:
\describe{ \item{\code{ti}}{Date and time the post was
published.} \item{\code{age}}{The age of the post as
difference between now and \code{ti}.} \item{\code{id}}{The
post's unique Google+ ID.} \item{\code{au}}{The post's
author's Google+ user ID.} \item{\code{ve}}{The action
describing the post.} \item{\code{nC}}{The number of
comments the post has attracted so far.}
\item{\code{nP}}{The number of +1s the post has attracted
so far.} \item{\code{nR}}{The number of reshared the post
has attracted so far.} \item{\code{nA}}{The number of
attachments to the post} \item{\code{msg}}{The post's
content.} }
}
\examples{
\dontrun{
  myPosts <- harvestPage("115046504166916768425", ret="list")
  myPosts.df <- ldply(myPosts, parsePost)
  }
}

