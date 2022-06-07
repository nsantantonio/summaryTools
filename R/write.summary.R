#' write.summary function
#'
#' function to (do something)
#'
#' @param locSum [value]
#' @param testName [value]
#' @param resultDir [value]
#' @param digits [value]. Default is 2
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
write.summary <- function(locSum, testName, resultDir, digits = 2, addLSD = FALSE, ...){
	if(addLSD){
		locSum$BLUE <- addPlusMinus(locSum$BLUE, ...)
		toRound <- grep("_sig", names(locSum$BLUE)) - 1
		locSum$BLUE[toRound] <- round(locSum$BLUE[toRound], digits = digits)
	} else {
		locSum$BLUE[-1] <- round(locSum$BLUE[-1], digits = digits)
	}
	locSum$BLUP[-1] <- round(locSum$BLUP[-1], digits = c(rep(digits, nrow(locSum$BLUP)-1), 2))
	write.csv(locSum$BLUE, paste0(resultDir, testName, "_BLUE.csv"), row.names = FALSE, quote = FALSE, na = "")
	write.csv(locSum$BLUP, paste0(resultDir, testName, "_BLUP.csv"), row.names = FALSE, quote = FALSE, na = "")
}
