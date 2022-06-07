#' roundSummary function
#'
#' function to round summaries. Only works if plus minus present 
#'
#' @param smry summary with mean, cv and lsd at end. Need plus minus to know which columns to round
#' @param dig1 use 1 digit for these traits that match this regex pattern
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
roundSummary <- function(smry, dig1 = "Test|Yield"){
	traitCols <- grep("_sig", names(smry))-1
	for(i in traitCols){
		if(grepl(dig1, names(smry)[i])) trdigits <- 1 else trdigits <- 0
		trdigits <- c(rep(trdigits, nrow(smry)-3), rep(1, 3))
		smry[[i]] <- round(smry[[i]], digits = trdigits)
	}
	return(smry)
}