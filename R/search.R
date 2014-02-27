##' Searching in Google+ Profiles
##' 
##' This function uses the Google+ API to search for a text string in profiles. 
##' Optionally, profiles can be restricted to a certain language.
##' 
##' @param q The query string to search.
##' @param language A language code. See 
##'   \url{https://developers.google.com/+/api/search#available-languages}.
##' @param maxPages Google fits 50 results on each page. This parameter 
##'   specifies how many pages to retrieve at most. By default, only the first
##'   page is being retrieved.
##' @param nextToken,page used internally to retrieve additional pages of 
##'   answers from the Google+ API. Users won't need to set this argument.
##' @return a data frame with the user ID and display names of the profiles that
##'   met the search criteria.
##' @export
##' @examples
##' \dontrun{
##' searchProfile("cats")
##' }
searchProfile <- function(q, language="en", maxPages=1, nextToken=NULL, page=1) {
  this.url <- paste0(base.url,
                     "people?query=",
                     q,
                     "&language=",
                     language,
                     "&maxResults=50",
                     nextToken,
                     "&key=",
                     .gpapikey)
  
  this.res <- fromJSON(getURL(this.url), asText=TRUE)
  this.ppl <- t(sapply(this.res[["items"]], function(x) data.frame(id=x$id, dn=x$displayName, stringsAsFactors=FALSE)))
  if (!is.null(this.res[["nextPageToken"]]) & page < maxPages) {
    this.nextToken <- paste0("&pageToken=", this.res[["nextPageToken"]])
    this.ppl <- rbind(this.ppl,
                  searchProfile(q, language, maxPages, this.nextToken, page+1))
  }
  return(this.ppl)
}