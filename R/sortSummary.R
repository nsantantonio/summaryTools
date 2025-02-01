#' sortSummary function
#'
#' function to sort summaries. 
#'
#' @param smry summary output from oneYearOneLocSummary, oneYearOverLocSummary, or multiYearSummary
#' @param traits list of traits to provide LSD from mean to 
#' @param decreasing should sort be done by decreasing values?
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
sortSummary <- function(smry, trait, decreasing = TRUE, ...){
	N <- grep("mean", smry$Line, ignore.case = TRUE) - 1
	dat <- smry[1:N,]
	stat <- smry[{N+1}:nrow(smry),]
	if(decreasing) sign <- -1 else sign <- 1
	datsort <- dat[order(sign* dat[[names(dat)[grep(trait, names(dat))][1]]]), ]
	rbind(datsort, stat)
}