#' addPlusMinus function
#'
#' function to add LSD separation from the mean
#'
#' @param smry summary output from oneYearOneLocSummary, oneYearOverLocSummary, or multiYearSummary
#' @param traits list of traits to provide LSD from mean to 
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
addPlusMinus <- function(smry, traits = NULL){
	# smry = usnMeans; traits = traits
	if(is.list(smry) & !is.data.frame(smry)) smry <- smry$BLUE
	rownames(smry) <- smry$Line

	avg <- grep("^mean$", smry$Line, ignore.case = TRUE)
	CV <- grep("^C\\.*V$", smry$Line, ignore.case = TRUE)
	LSD <- grep("^L\\.*S\\.*D$", smry$Line, ignore.case = TRUE)

	stat <- smry[c(avg, LSD, CV),]
	rownames(stat) <- c("mean", "LSD", "CV")
	lastL <- avg -1 
	BLUE <- smry[1:lastL,]

	if(is.null(traits)){
		traits <- colnames(stat[sapply(stat, function(x) is.numeric(x) & all(!is.na(x)))])
	}
	trtCols <- NULL
	# for(i in traits) trtCols[i] <- which(i == names(BLUE))
	for(i in traits) trtCols[i] <- grep(paste0("^", gsub("\\|.*", "", i)), names(BLUE))
	firstTrt <- min(trtCols)
	
	BLUEinfo <- BLUE[!names(BLUE) %in% traits]
	statinfo <- stat[!names(stat) %in% traits]

	trtL <- list()
	statL <- list()
	counter <- 1
	for(i in traits){
		bi <- data.frame(BLUE[trtCols[i]], check.names = FALSE)
		bi[[paste0(i, "_sig")]] <- ""
		if(!is.na(stat["LSD", trtCols[i]])){
			bi[[paste0(i, "_sig")]][BLUE[[trtCols[i]]] > stat["mean", trtCols[i]] + stat["LSD", trtCols[i]]] <- "+"
			bi[[paste0(i, "_sig")]][BLUE[[trtCols[i]]] < stat["mean", trtCols[i]] - stat["LSD", trtCols[i]]] <- "-"
		}
		trtL[[counter]] <- bi

		si <- data.frame(stat[trtCols[i]], check.names = FALSE)
		si[[paste0(i, "_sig")]] <- ""
		statL[[counter]] <- si
		counter <- counter + 1
	}
	BLUEpm <- data.frame(BLUEinfo, do.call(cbind,trtL), check.names = FALSE)
	statpm <- data.frame(statinfo, do.call(cbind,statL), check.names = FALSE)
	rbind(BLUEpm, statpm)
}