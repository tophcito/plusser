\name{parsePost}
\alias{parsePost}
\title{Parsing a Google+ post}
\usage{
parsePost(p)
}
\arguments{
  \item{p}{A raw post as returned from e.g. the
  \code{\link{harvestPage}} function.}
}
\value{
A 1 row data frame filled with the information from the
post parsed.
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
difference between now and \code{ti} in (floating point)
days.} \item{\code{id}}{The post's unique Google+ post ID.}
\item{\code{au}}{The post's author's Google+ user ID.}
\item{\code{ve}}{The action describing the post.}
\item{\code{nC}}{The number of comments the post has
attracted so far.} \item{\code{nP}}{The number of +1s the
post has attracted so far.} \item{\code{nR}}{The number of
times the post has been reshared so far.}
\item{\code{at}}{The type of attachment (article, photo,
...) as reported by the API} \item{\code{msg}}{The post's
content.} }
}
\examples{
\dontrun{
myPosts <- harvestPage("115046504166916768425", ret="list")
myPosts.df <- ldply(myPosts, parsePost)
}
}

