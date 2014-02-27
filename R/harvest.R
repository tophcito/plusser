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
##' @param maxPages Google fits 100 results on each age. This parameter specifies how many
##'   pages to retrieve at most. By default, only the first page is being
##'   retrieved.
##' @param nextToken,page used internally to retrieve additional pages of
##'   answers from the Google+ API. Users won't need to set these arguments.
##' @return The function returns a list or a data frame. See \code{Details} for
##'   more on its content.
##' @export
#' @importFrom plyr ldply
##' @examples
##' \dontrun{
##' myPosts.df <- harvestPage("115046504166916768425")
##' }
harvestPage <- function(user, ret="data.frame", maxPages=1, nextToken=NULL, page=1) {
  url <- paste0(base.url,
                start.people,
                user,
                close.page,
               .gpapikey)
  this.res <- fromJSON(getURL(url), asText=TRUE)
  res <- this.res[["items"]]
  if(!is.null(this.res[["nextPageToken"]]) & page < maxPages) {
    this.nextToken <- paste0("&pageToken=", this.res[["nextPageToken"]])
    res <- c(res, harvestPage(user, "list", maxPages, this.nextToken, page+1))
  }
  if (ret=="list") {
    return(res)
  } else {
    res <- ldply(res, parsePost)
    return(res)
  }
}


##' Retrieve the users that acted on a G+ post
##'
##' This function retrieves the users that either +1ed or reshared a post.
##'
##' @param activity the post ID for which the users should be retrieved.
##' @param kind denoting the kind of person to be retrieved. Either
##'   \code{plusoners} or \code{resharers}.
##' @param nextToken used internally to retrieve additional pages of answers
##'   from the Google+ API. Users won't need to set this argument.
##' @return Returns a (character) vector of Google+ user IDs.
##' @export
harvestActivity <- function(activity, kind=c("plusoners", "resharers"),
                            nextToken=NULL) {
  this.url <- paste0(base.url, "activities/",
                     activity,
                     "/",
                     start.people,
                     kind,
                     "?maxResults=100",
                     nextToken,
                     "&key=",
                     .gpapikey)
  this.res <- fromJSON(getURL(this.url), asText=TRUE)
  this.ppl <- sapply(this.res[["items"]], function(x) x$id)
  if (!is.null(this.res[["nextPageToken"]])) {
    this.nextToken <- paste0("&pageToken=", this.res[["nextPageToken"]])
    this.ppl <- c(this.ppl,
                  harvestActivity(activity, kind, this.nextToken))
  }
  return(this.ppl)
}


##' Retrieve the profile of a Google+ user
##'
##' This function retrieves the profile of a Google+ user. The results are
##' returned in a data frame. See \code{Details}.
##'
##' The following fields will be filled with data (if available) and \code{NA}
##' otherwise:
##' \describe{
##'   \item{\code{id}}{The Google+ user ID.}
##'   \item{\code{sex}}{The user's gender: \code{male}, \code{female}, or 
##'                     \code{other}.}
##'   \item{\code{ln}}{The user's last name.}
##'   \item{\code{fn}}{The user's first name.}
##'   \item{\code{verified}}{Logical. \code{TRUE} if it is a verified Google+ 
##'                          profile.}
##'   \item{\code{ageMin, ageMax}}{Google+ provides only age ranges. This will
##'                                contain the lower and upper bound of the age
##'                                range of the user.}
##'   \item{\code{bday}}{The birthday of the user (YYYY-MM-DD).}
##'   \item{\code{nCircled}}{The number of Persons the user circled by.}
##'   \item{\code{currentLoc}}{The user's current location.}
##'   \item{\code{lang}}{The primary language the user reported.}
##'   \item{\code{p1count}}{The number of +1s the user awarded.}
##'   \item{\code{relationship}}{The user's relationship status.}
##'   \item{\code{bio}}{The `About Me' short autobiography.}
##'   \item{\code{tagline}}{The tagline of a profile.}
##'   \item{\code{type}}{The type of a profile: \code{person} or \code{page}.}
##'   \item{\code{brag}}{The `bragging rights' section of the profile.}
##'   \item{\code{occ}}{The person's occupation.}
##'   \item{\code{skills}}{Ther person's skills.}
##'   }
##'
##'   @param id the Google+ user ID.
##'   @return The function returns a 1-row data frame with all available
##'           information. See \code{Details} for a description of its columns.
##'   @export
harvestProfile <- function(id) {
  this.url <- paste0(base.url,
                     start.people,
                     id,
                     close.people,
                     .gpapikey)
  this.res <- fromJSON(getURL(this.url), asText=TRUE)
  this.ext <- list(id=this.res$id,
                   sex=this.res$gender,
                   ln=this.res$name[1],
                   fn=this.res$name[2],
                   verified=this.res$verified,
                   ageMin=this.res$ageRange[2],
                   ageMax=this.res$ageRange[1],
                   bday=this.res$birthday,
                   nCircled=this.res$circledByCount,
                   currentLoc=this.res$currentLocation,
                   lang=this.res$language,
                   p1count=this.res$plusOneCount,
                   relationship=this.res$relationshipStatus,
                   bio=this.res$aboutMe,
                   tagline=this.res$tagline,
                   type=this.res$type,
                   brag=this.res$braggingRights,
                   occ=this.res$occupation,
                   skills=this.res$skills)
  this.ext[sapply(this.ext, is.null)] <- NA
  this.ext <- as.data.frame(this.ext)
  rownames(this.ext) <- NULL
  return(this.ext)
}
