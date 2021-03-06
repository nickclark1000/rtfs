% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/release_summary.R
\name{get_release_sprints}
\alias{get_release_sprints}
\title{Get Release Sprints}
\usage{
get_release_sprints(release_name)
}
\arguments{
\item{release_name}{Name of the release/iteration as defined in TFS}
}
\value{
A list of sprint iterations
}
\description{
Get a list of sprints in a particular release/iteration.
}
\examples{
get_release_sprints('Version 2.0')
}

