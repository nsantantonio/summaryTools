#' formatPrintSummary function
#'
#' function to sort summaries. 
#'
#' @param smry summary output from oneYearOneLocSummary, oneYearOverLocSummary, or multiYearSummary
#' @param trait trait to calculate percent of mean and rank
#' @param keep columns to keep at begining of data frame, PercMean and Rank will be added to beginning of data.frame
#' @param calcPercMean should PercMean be calculated?
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
addTraitPercRank <- function(smry, trait, keep = "Line", calcPercMean = TRUE, sign = -1, ...){
	di <- 1:{nrow(smry)-3}
	si <- {nrow(smry)-2}:{nrow(smry)-3}
	smryL <- smry[keep]
	whichTrait <- grep(trait, names(smry))[1]
	if(calcPercMean) smryL[[paste0(gsub("\\..*", "", trait), "PercMean")]] <- as.integer(c(round(smry[[whichTrait]][di] / smry[[whichTrait]][si[1]] *100), rep(NA, 3)))
	smryL[[paste0(gsub("\\..*", "", trait), "Rank")]] <- c(rank(sign*smry[[whichTrait]][di], ...), rep(NA, 3))
	data.frame(smryL, smry[!names(smry) %in% keep], check.names = FALSE)
}