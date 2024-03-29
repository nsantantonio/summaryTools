% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanHarvM.R
\name{cleanHarvM}
\alias{cleanHarvM}
\title{cleanHarvM function}
\usage{
cleanHarvM(
  hmi,
  blockName,
  lbs = TRUE,
  forgotCycleThreshold = 0.2,
  lowMoistThreshold = 9,
  qnt = 0.99,
  keepCols = NULL,
  keepSeq = FALSE,
  rmPlotPattern = NULL
)
}
\arguments{
\item{hmi}{[value]}

\item{lbs}{[value]. Default is TRUE}

\item{forgotCycleThreshold}{[value]. Default is 0.2}

\item{lowMoistThreshold}{threshold for too low moisture. records with low moisture will be set to NA. Default is 9}

\item{qnt}{[value]. Default is 0.99}
}
\value{
[value]
}
\description{
function to (do something)
}
\details{
[fill in details here]
}
\examples{
# none
}
