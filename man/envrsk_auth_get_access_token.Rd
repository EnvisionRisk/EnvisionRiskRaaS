% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/envrsk_api_bridge_2_R.R
\name{envrsk_auth_get_access_token}
\alias{envrsk_auth_get_access_token}
\title{Function to get access token for API authentication}
\usage{
envrsk_auth_get_access_token(usr_id, usr_pwd)
}
\arguments{
\item{usr_id}{A string containing the user ID.}

\item{usr_pwd}{A string containing the user password.}
}
\value{
A list containing the status code, the access token and its expiry time.
        In case of error, it returns the error message.
}
\description{
This function obtains an access token needed for API authentication.
It requires a user ID and password, and sends a GET request to the
specified URL to retrieve the access token. If the request is successful,
it sets the expiry time for the access token to 24 hours from the current time.
}
\examples{
\dontrun{
token <- envrsk_auth_get_access_token(usr_id = "your_user_id",
                                      usr_pwd = "your_password")
}
}
\keyword{internal}
