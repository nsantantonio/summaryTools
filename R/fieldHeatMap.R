#' fieldHeatmap function
#' 
#' function to draw heatmaps of field trials
#'
#' @param dF data.frame of field trial, 
#' @param triat data.frame of field trial, with traits and row and column variables
#' @param rVar character. data.frame variable name to indicate row. Default is 'range'
#' @param cVar character. data.frame variable name to indicate column. Default is 'pass'
#' @return summary table with notes added, and no stats lost.
#' @details [fill in details here]
#' @examples betterImage(volcano)
#' @export
fieldHeatmap <- function(dF, trait, cVar = "pass", rVar = "range", ...){
	# dF = prelimAR; trait = "GrainYield"; col = magma(50); cVar = "pass"; rVar = "range"
	par(bg = "gray")
	par(mar = c(0, 0, 4, 0))
	dF <- dF[!is.na(dF[[rVar]]) & !is.na(dF[[cVar]]), ]
	traitMat <- matrix(NA, max(dF[[rVar]], na.rm = TRUE), max(dF[[cVar]], na.rm = TRUE))
	traitMat[cbind(dF[[rVar]], dF[[cVar]])] <- dF[[trait]]
	betterImage(traitMat[nrow(traitMat):1, ], xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n", ...)
}
