if(getRversion() >= "2.15.1")  utils::globalVariables(c(".gpapikey")) ##to make CRAN happy


##' Sets an API key for the Google+ API
##' 
##' This function sets an API key that is then stored invisibly, for
##' \code{plusser} to use when accessing the Google+ API.
##' 
##' @param apikey the API key as a character string.
##' @return Returns \code{TRUE} if the key was stored successfully.
##' @export
##' @examples
##' setAPIkey("thisIsInvalid")
setAPIkey <- function(apikey) {
  assign(".gpapikey", apikey, envir=.GlobalEnv)
  return(isTRUE(all.equal(.gpapikey,apikey)))
}
