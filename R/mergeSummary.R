#' readRegionalExcel function
#'
#' function to read Wade's crazy excel table summaries. 
#'
#' @param x first summary to be merged
#' @param y second summary to be merged
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export

mergeSummary <- function(x, y, ...){
	mergedSum <- merge(x, y, ...)
	rbind(mergedSum[!mergedSum$Line %in% c("mean", "CV", "LSD"), ], 
			mergedSum[mergedSum$Line == "mean", ], 
			mergedSum[mergedSum$Line == "CV", ],
			mergedSum[mergedSum$Line == "LSD", ])
}