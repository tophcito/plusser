##' Parsing a Google+ post
##' 
##' This function turns a Google+ post into a (1 row) data frame extracting or
##' computing a number of fields. See \code{Details}.
##' 
##' This function extracts or computes the following fields:
##' \describe{
##'   \item{\code{ti}}{Date and time the post was published.}
##'   \item{\code{age}}{The age of the post as difference between now and \code{ti}.}
##'   \item{\code{id}}{The post's unique Google+ ID.}
##'   \item{\code{au}}{The post's author's Google+ user ID.}
##'   \item{\code{ve}}{The action describing the post.}
##'   \item{\code{nC}}{The number of comments the post has attracted so far.}
##'   \item{\code{nP}}{The number of +1s the post has attracted so far.}
##'   \item{\code{nR}}{The number of reshared the post has attracted so far.}
##'   \item{\code{nA}}{The number of attachments to the post}
##'   \item{\code{msg}}{The post's content.}   
##'   }
##'   
##'   @param p a raw post as returned from e.g. the \code{\link{harvestPage}} 
##'   function.
##'   @return A data frame with the information from the post parsed.
##'   @export
##'   @examples
##'   \dontrun{
##'   myPosts <- harvestPage("115046504166916768425", ret="list")
##'   myPosts.df <- ldply(myPosts, parsePost)
##'   }
parsePost <- function(p) { #use for all children of 6. element of retrieved json list
  ti <- ymd_hms(p$published)
## TODO: look up location of user; use to look up timezone and set accordingly.
  tod <- hour(ti)
  if (tod >= 6 & tod < 9) {
    todf <- "earlyMorning"
  } else if (tod >= 9 & tod < 12) {
    todf <- "lateMorning"
  } else if (tod >= 12 & tod < 17) {
    todf <- "afternoon"
  } else if (tod >= 17 & tod < 20) {
    todf <- "evening"
  } else if (tod >= 20 | tod < 1) {
    todf <- "night"
  } else {
    todf <- "sleep"
  }
  id <- p$id
  au <- p$actor$id
  ve <- p$verb
  msg <- as.character(p$object$content)
  msg <- gsub("<.*?>", "", msg) ## removing all HTML tags from a G+ msg
  nC <- p$object$replies$totalItems
  nP <- p$object$plusoners$totalItems
  nR <- p$object$resharers$totalItems
  nA <- length(p$object$attachments)
  df <- data.frame(ti=ti,
                   age=as.numeric(now() - ti),
                   id=id,
                   au=au,
                   ve=ve,
                   nC=nC,
                   nP=nP,
                   nR=nR,
                   nA=nA,
                   msg=msg)
  return(df)
}
