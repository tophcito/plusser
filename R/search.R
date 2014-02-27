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


##' Searching for Google+ Posts
##' 
##' This function uses the Google+ API to search for a text string in posts. 
##' Optionally, search results can be restricted to a certain language.
##' 
##' The result is either a simple list of items from the page that can be parsed
##' using \code{\link{parsePost}} or a data frame with that function allready 
##' applied.
##' 
##' The length of the list or the number of rows of the data frame are somewhat 
##' ambigious. Specifying the \code{results} argument will try to get that many 
##' results. But there may be less (because Google could not find more) or more 
##' (because Google is organizing results on pages and it would be a waste to 
##' discard them automatically). If you really depend on getting not more rows 
##' than you expected, use standard selection (i.e. \code{[}) to trim the
##' results.
##' 
##' @param q The query string to search.
##' @param ret A string specifying the kind of return value. Either a 
##'   \code{list} of the rerieved items on the page, or that list parsed into a 
##'   \code{data.frame}.
##' @param language A language code. See 
##'   \url{https://developers.google.com/+/api/search#available-languages}.
##' @param results The approximate number of results that will be retrieved from
##'   Google+
##' @param nextToken,cr used internally to retrieve additional pages of answers 
##'   from the Google+ API. Users won't need to set this argument.
##' @return The function returns a list or a data frame containing all available
##'   data on the posts that met the search criteria. See \code{Details} for 
##'   more on its content.
##' @export
##' @examples
##' \dontrun{
##' searchPost("#cats")
##' }
searchPost <- function(q, ret="data.frame", language=NULL, results=1, nextToken=NULL, cr=0) {
  q <- curlEscape(q)
  if (is.null(language)) {
    languageString <- NULL
  } else {
    languageString <- paste0("&language=", language)
  }
  url <- paste0(base.url,
                "activities?query=",
                q,
                languageString,
                "&maxResults=20",
                nextToken,
                "&key=",
                .gpapikey)
  this.res <- fromJSON(getURL(url), asText=TRUE)
  res <- this.res[["items"]]
  cr <- cr + length(res)
  if(!is.null(this.res[["nextPageToken"]]) & cr < results) {
    this.nextToken <- paste0("&pageToken=", this.res[["nextPageToken"]])
    res <- c(res, searchPost(q, ret="list", language, results, this.nextToken, cr+length(res)))
  }
  if (ret=="list") {
    return(res)
  } else {
    res <- ldply(res, parsePost)
    return(res)
  }
}