#' formatPrintSummary function
#'
#' function to sort summaries. 
#'
#' @param smry summary output from oneYearOneLocSummary, oneYearOverLocSummary, or multiYearSummary
#' @param infoCols list of col names to be included in printed list as is
#' @param tex should a latex table be printed to stdout?
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
formatPrintSummary <- function(smry, infoCols = "Line", tex = TRUE){
	# smry = sumipm; infoCols = c("Line", "Entry"); tex = FALSE
	if(tex) require(xtable)
	info <- as.matrix(smry[infoCols])
	traits <- grep("_sig", names(smry)) - 1
	sig <- grep("_sig", names(smry))
	otherCols <- 1:length(smry)
	otherCols <- otherCols[!otherCols %in% c(traits, sig, which(names(smry) %in% infoCols))]
	sigpr <- as.matrix(smry[sig])
	sigpr[is.na(sigpr)] <- ""
	traitpr <- as.matrix(sapply(smry[traits], sprintf, fmt = "%.4g"))
	traitpr[traitpr == "NA"] <- ""
	otherpr <- as.matrix(smry[otherCols])
	prmat <- matrix(paste0(traitpr, sigpr), nrow = nrow(traitpr), ncol = ncol(traitpr), dimnames = dimnames(traitpr))
	if(!is.null(infoCols)) prmat <- cbind(info, prmat, otherpr)
	if(tex)  print(xtable(prmat), include.rownames = FALSE)  else return(prmat)
}
