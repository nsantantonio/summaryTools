% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataToFb.R
\name{dataToFb}
\alias{dataToFb}
\title{dataToFb function}
\usage{
dataToFb(
  dF,
  traits,
  plot_name = "plot_name",
  timestamp = NULL,
  person = NULL,
  location = NULL,
  number = 1
)
}
\arguments{
\item{dF}{data.frame with plot_name and triat columns. This would tpicaly be data recorded on a spreadshet for whatever reason.}

\item{traits}{chaacter vector of trait names coresponding to dF columns}

\item{plot_name}{character vector of length 1, Default is 'plot_name'}

\item{timestamp}{Not integrated yet. Default is NULL}

\item{person}{Not integrated yet. Default is NULL}

\item{location}{Not integrated yet. Default is NULL}

\item{number}{Not integrated yet. Default is 1}
}
\value{
[value]
}
\description{
function to convert a data.frame of data o fieldook database file. Usual used to get data from colaboators, etc, anyting that was recored on a spreadshet.
}
\details{
[fill in details here]
}
\examples{
# none
}
