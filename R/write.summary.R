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
#' @examples # none
#' @export
write.summary <- function(locSum, testName, resultDir, digits = 2, addLSD = FALSE, ...){
	# locSum = locSummary[[k]][[j]]; testName = j; resultDir = resultDir; digits = 2; addLSD = FALSE
	# locSum = results; testName = "WheatlandSpring_2022_WAFVA"; resultDir = "results/"; digits = 2; addLSD = FALSE
	if(length(locSum) == 1 & !any(names(locSum) %in% c("BLUE", "BLUP"))) {
		if(any(names(locSum[[1]]) %in% c("BLUE", "BLUP"))){
			locSum <- locSum[[1]]
		}
	}
	if(addLSD){
		locSum$BLUE <- addPlusMinus(locSum$BLUE, ...)
		toRoundBLUE <- grep("_sig", names(locSum$BLUE)) - 1
		locSum$BLUE[toRoundBLUE] <- round(locSum$BLUE[toRoundBLUE], digits = digits)
	} else {
		isNumBLUE <- which(sapply(locSum$BLUE, is.numeric))
		toRoundBLUE <- isNumBLUE[!names(isNumBLUE) %in% "Entry"]
		locSum$BLUE[toRoundBLUE] <- round(locSum$BLUE[toRoundBLUE], digits = digits)
	}
	isNumBLUP <- which(sapply(locSum$BLUP, is.numeric))
	toRoundBLUP <- isNumBLUP[!names(isNumBLUP) %in% "Entry"]
	locSum$BLUP[toRoundBLUP] <- round(locSum$BLUP[toRoundBLUP], digits = c(rep(digits, nrow(locSum$BLUP)-1), 2))
	write.csv(locSum$BLUE, paste0(resultDir, testName, "_BLUE.csv"), row.names = FALSE, na = "", ...)
	write.csv(locSum$BLUP, paste0(resultDir, testName, "_BLUP.csv"), row.names = FALSE, na = "", ...)
}
