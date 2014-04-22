\name{parseAttachments}
\alias{parseAttachments}
\title{Parsing of post attachments}
\usage{
parseAttachments(pl)
}
\arguments{
  \item{pl}{a posting list as retrieved e.g. by
  \code{\link{harvestPage}}.}
}
\value{
A list containing one data frame per identified attachment
type.
}
\description{
This function takes a raw list of posts (as retrieved by
\code{\link{harvestPage}}) and extracts any attachments it
might find. It uses private (i.e. not exported) parsing
functions for some known attachment types. At present,
these are articles, albums, photos and videos. Other
attachment types will just be cast generically to
data.frames. The rownames of all these data frames are the
\code{id}s of the posts that attachment belongs to. The
columns of the returned data frames should be pretty much
self-explanatory. If in doubt, check the Google+ API
documentation
\url{https://developers.google.com/+/api/latest/activities#object.attachments}.
}
\examples{
\dontrun{
myPosts <- harvestPage("115046504166916768425", ret="list")
myPosts.att <- parseAttachments(myPosts)
}
}

