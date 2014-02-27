##' Retrieve the posts of a user's G+ page.
##' 
##' This function retrieves up to the 100 last posts that a user put on her 
##' page.
##' 
##' The result is either a simple list of items from the page that can be parsed
##' using \code{\link{parsePost}} or a data frame with that function allready 
##' applied.
##' 
##' @param user The UID of a user.
##' @param ret A string specifying the kind of return value. Either a 
##'   \code{list} of the rerieved items on the page, or that list parsed into a 
##'   \code{data.frame}.
##' @return The function returns a list or a data frame. See \code{Details} for
##'   more on its content.
##' @export
#' @importFrom plyr ldply
##' @examples
##' \dontrun{
##' myPosts.df <- harvestPage("115046504166916768425")
##' }
harvestPage <- function(user, ret="data.frame") {
  url <- paste0(base.url,
                start.people,
                user,
                close.page,
               .gpapikey)
  res <- fromJSON(getURL(url), asText=TRUE)
  if (ret=="list") {
    return(res[["items"]])
  } else {
    res <- ldply(res[["items"]], parsePost)
    return(res)
  }
}
