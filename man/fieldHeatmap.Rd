% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fieldHeatMap.R
\name{fieldHeatmap}
\alias{fieldHeatmap}
\title{fieldHeatmap function}
\usage{
fieldHeatmap(dF, trait, cVar = "pass", rVar = "range", ...)
}
\arguments{
\item{dF}{data.frame of field trial,}

\item{cVar}{character. data.frame variable name to indicate column. Default is 'pass'}

\item{rVar}{character. data.frame variable name to indicate row. Default is 'range'}

\item{triat}{data.frame of field trial, with traits and row and column variables}
}
\value{
summary table with notes added, and no stats lost.
}
\description{
function to draw heatmaps of field trials
}
\details{
[fill in details here]
}
\examples{
betterImage(volcano)
}
