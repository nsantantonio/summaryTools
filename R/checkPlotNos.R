#' checkPlotNos function
#'
#' function to (do something)
#'
#' @param x [value]
#' @param type [value]
#' @param testName [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
checkPlotNos <- function(x, type, testName = NULL){
	if(length(x) == 0){stop("No ", type, " Records! Check input file.")}
	if(class(x) == "character") numx <- as.numeric(x) else numx <- x
	if(class(x) %in% c("numeric", "integer")) x <- as.character(x)
	minx <- min(numx)
	maxx <- max(numx)
	if (minx > 100){
		reps <- substr(x, 1, 1)
		# tabRep <- table(reps)
		ncharx <- unique(nchar(x))
		if(length(ncharx) > 1) {
			message("Unexpected plot numbering system. unexpected results may follow. Please check plot numbers. ")
			ncharx <- max(ncharx)
		}
		nReps <- as.numeric(unique(reps))
		maxEnt <- max(as.numeric(substr(x, 2, ncharx)))
		if (ncharx == 4) expon <- 1e3 else expon <- 1e2
		allPlots <- unlist(lapply(nReps, function(x) x * expon + 1:maxEnt))
	} else {
		allPlots <- 1:maxx
	}
	miss <- allPlots[!allPlots %in% numx]
	if(length(miss)){
		message("Missing plots detected! The following plots are not in the ", type, " dataset! Please check other files for these plots (or that they existed, e.g. JoMar doesnt have consecutive plots)")
		if(!is.null(testName)) print(testName)
		for(m in miss) print(m)
		return(miss)
	}
}
