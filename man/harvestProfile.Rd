\name{harvestProfile}
\alias{harvestProfile}
\title{Retrieve the profile of Google+ users}
\usage{
harvestProfile(id)
}
\arguments{
  \item{id}{A character vector of the Google+ user ID(s).}
}
\value{
The function returns a data frame with all available
information with one row per user ID. See \code{Details}
for a description of its columns.
}
\description{
This function retrieves the profile of one or more Google+
user(s). Google calls this `get people`. The results are
returned in a data frame. See \code{Details}.
}
\details{
The following fields will be filled with data (if
available) or \code{NA} otherwise: \describe{
\item{\code{id}}{The Google+ user ID.}
\item{\code{sex}}{The user's gender: \code{male},
\code{female}, or \code{other}.} \item{\code{ln}}{The
user's last name.} \item{\code{fn}}{The user's first name.}
\item{\code{verified}}{Logical. \code{TRUE} if it is a
verified Google+ profile.} \item{\code{ageMin,
ageMax}}{Google+ provides only age ranges for some
profiles. This will contain the lower and upper bound of
the age range of the user.} \item{\code{bday}}{The birthday
of the user (YYYY-MM-DD).} \item{\code{nCircled}}{The
number of Persons the user circled by.}
\item{\code{currentLoc}}{The user's current location.}
\item{\code{lang}}{The primary language the user reported.}
\item{\code{p1count}}{The number of +1s the user awarded.}
\item{\code{relationship}}{The user's relationship status.}
\item{\code{bio}}{The `About Me' short autobiography.}
\item{\code{tagline}}{The tagline of a profile.}
\item{\code{type}}{The type of a profile: \code{person} or
\code{page}.} \item{\code{brag}}{The `bragging rights'
section of the profile.} \item{\code{occ}}{The person's
occupation.} \item{\code{skills}}{The person's skills.} }
}
\examples{
\dontrun{
gProfile <- harvestProfile("+google")
}
}
\seealso{
Google+ API documentation:
\url{https://developers.google.com/+/api/latest/people/get}
}

