##' Retrieve the posts of a user's G+ page
##' 
##' This function retrieves the most recent posts that a user put on her 
##' page. Google calls this `listing activities`.
##' 
##' The result is either a simple list of items from the page that can be parsed
##' using \code{\link{parsePost}} or a data frame with that function already
##' applied.
##' 
##' The length of the list or the number of rows of the data frame are somewhat 
##' ambiguous. Specifying the \code{results} argument will try to get that many
##' results. But there may be less (because Google could not find more) or more 
##' (because Google is organizing results on pages and it would be a waste to 
##' discard them automatically). If you really depend on getting not more rows 
##' than you expected, use standard selection (i.e. \code{[}) to trim the
##' results.
##' 
##' @param user A user identification string: either user ID or +Name.
##' @param ret A string specifying the kind of return value. Either a 
##'   \code{list} of the retrieved items on the page, or that list parsed into a
##'   \code{data.frame}.
##' @param results The approximate number of results that will be retrieved from
##'   Google+.
##' @param nextToken,cr used internally to retrieve additional pages of
##'   answers from the Google+ API. Users won't need to set these arguments.
##' @return The function returns a list or a data frame. See \code{Details} for
##'   more on its content.
##' @export
##' @seealso Google+ API documentation:
##'   \url{https://developers.google.com/+/api/latest/activities/list}.
#' @importFrom plyr ldply
##' @examples
##' \dontrun{
##' myPosts.df <- harvestPage("115046504166916768425")
##' gPosts.df <- harvestPage("+google", results=200)
##' }
harvestPage <- function(user, ret="data.frame", results=1, nextToken=NULL, cr=1) {
  if (!exists(".gpapikey")) stop("Set the Google+ API key first using setAPIkey().")
  if (results < 1) stop("Argument 'results' needs be positive.")
  if (ret != "data.frame" & ret != "list") stop("Argument 'ret' must be either 'data.frame' or 'list'")
  url <- paste0(base.url,
                start.people,
                curlEscape(user),
                close.page,
               .gpapikey)
  this.res <- fromJSON(getURL(url), asText=TRUE)
  res <- this.res[["items"]]
  cr <- cr + length(res)
  if(!is.null(this.res[["nextPageToken"]]) & cr < results) {
    this.nextToken <- paste0("&pageToken=", this.res[["nextPageToken"]])
    res <- c(res, harvestPage(user, "list", results, this.nextToken, cr))
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
##' Google calls this `list by activity`.
##'
##' @param activity The post ID for which the users should be retrieved.
##' @param kind Denoting the kind of person to be retrieved. Either
##'   \code{plusoners} or \code{resharers}.
##' @param nextToken This is used internally to retrieve additional pages of
##'   answers from the Google+ API. Users won't need to set this argument.
##' @return Returns a (character) vector of Google+ user IDs.
##' @seealso Google+ API documentation:
##'   \url{https://developers.google.com/+/api/latest/people/listByActivity}.
##' @export
##' @examples
##' \dontrun{
##' ## User IDs of people that +1ed this post
##' users.p <- harvestActivity("z131ihvycs30ivrxm04cjbiwjkbqujka0sk0k", "plusoners")
##' }
harvestActivity <- function(activity, kind=c("plusoners", "resharers"),
                            nextToken=NULL) {
  if (!exists(".gpapikey")) stop("Set the Google+ API key first using setAPIkey().")
  if (kind != "plusoners" & kind != "resharers") stop("Argument 'kind' needs to be either 'plusoners' or 'resharers'.")
  this.url <- paste0(base.url, "activities/",
                     curlEscape(activity),
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


##' Retrieve the profile of Google+ users
##'
##' This function retrieves the profile of one or more Google+ user(s). Google
##' calls this `get people`. The results are returned in a data frame. See 
##' \code{Details}.
##'
##' The following fields will be filled with data (if available) or \code{NA}
##' otherwise:
##' \describe{
##'   \item{\code{id}}{The Google+ user ID.}
##'   \item{\code{sex}}{The user's gender: \code{male}, \code{female}, or 
##'                     \code{other}.}
##'   \item{\code{ln}}{The user's last name.}
##'   \item{\code{fn}}{The user's first name.}
##'   \item{\code{verified}}{Logical. \code{TRUE} if it is a verified Google+ 
##'                          profile.}
##'   \item{\code{ageMin, ageMax}}{Google+ provides only age ranges for some
##'                                profiles. This will contain the lower and
##'                               upper bound of the age range of the user.}
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
##'   \item{\code{skills}}{The person's skills.}
##'   }
##'
##' @param id A character vector of the Google+ user ID(s).
##' @return The function returns a data frame with all available 
##'   information with one row per user ID. See \code{Details} for a description
##'   of its columns.
##' @seealso Google+ API documentation:
##'   \url{https://developers.google.com/+/api/latest/people/get}
##' @export
##' @examples
##' \dontrun{
##' gProfile <- harvestProfile("+google")
##' }
harvestProfile <- function(id) {
  if (!exists(".gpapikey")) stop("Set the Google+ API key first using setAPIkey().")
  if (length(id)>1) {
    res <- ldply(as.list(id), harvestProfile)
    return(res)
  } else {
  this.url <- paste0(base.url,
                     start.people,
                     curlEscape(id),
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
  this.ext <- as.data.frame(this.ext, stringsAsFactors=FALSE)
  rownames(this.ext) <- NULL
  return(this.ext)
  }
}
