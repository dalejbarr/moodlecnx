% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/fetch.R
\name{fetch}
\alias{fetch}
\title{Download assignments from moodle}
\usage{
fetch(moodle_url, assign_id, targ_dir = NULL, overwrite = FALSE)
}
\arguments{
\item{moodle_url}{web address of moodle server (must already be logged in)}

\item{assign_id}{assignment id number; digits only}

\item{targ_dir}{subdirectory in which to store downloaded submissions; if \code{NULL} will be downloaded to a subdirectory of the working directory named \code{assign_id}}

\item{overwrite}{overwrite the subdirectory if it exists (default \code{FALSE})}
}
\value{
a tibble containing information about contents of the downloaded directory (see \code{\link{describe}})
}
\description{
\code{fetch} downloads student submissions for a given assignment from a moodle server.
}
\seealso{
\code{\link{login}}, \code{\link{describe}}
}

