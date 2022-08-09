#' trialDesignToDataFrame function
#'
#' function to (do something)
#'
#' @param tr object of class trialDesign.
#' @return data.frame with plot_name, trialName, Year, Location, Line, Entry, and Block.
#' @details [fill in details here]
#' @examples none
#' @export
trialDesignToDataFrame <- function(tr) {
	trialSplit <- strsplit(tr@plotName, "_")
	test <- sapply(trialSplit, "[", 1)
	yr <- sapply(trialSplit, "[", 2)
	loc <- sapply(trialSplit, "[", 3)

	trdf <- data.frame(plot_name = tr@plotName, 
					   trialName = tr@trialName,
					   test = test,
					   Year = yr, 
					   Location =loc,
					   Line = tr@Line, 
					   Entry = tr@Entry, 
					   Block = tr@block, 
					   Plot = tr@plotNo)
	return(trdf)
}