#' cleanHarvM function
#'
#' function to (do something)
#'
#' @param hmi [value]
#' @param lbs [value]. Default is TRUE
#' @param forgotCycleThreshold [value]. Default is 0.2
#' @param qnt [value]. Default is 0.99
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
cleanHarvM <- function(hmi, lbs = TRUE, forgotCycleThreshold = 0.2, qnt = 0.99, keepCols = NULL){
	# lbs = TRUE; forgotCycleThreshold = 0.2; qnt = 0.99; keepCols = NULL
	hmi <- hmi[order(hmi[[grep("Sequence", names(hmi))]]),]
	head(hmi) 
	hmi <- hmi[!hmi[[grep("Line", names(hmi))]] %in% "fill",]
	cols <- sapply(c("^plot.*id", "^moisture", "^weight", "^test", "note"), function(x) grep(x, names(hmi), ignore.case = TRUE))
	hm <- hmi[cols]
	names(hm) <- c("plot_name", "Moisture", "netWeight", "TestWeight", "Notes")
	if(!is.null(keepCols)) hm[keepCols] <- hmi[keepCols]
	badCycle <- which(hm[["netWeight"]] < forgotCycleThreshold)


	if(length(badCycle)){
		qthresh <- quantile(hm[, "netWeight"], probs = qnt, na.rm = TRUE)

		for (j in badCycle){
			# hm[c(j-1, j), c("netWeight", "Moisture", "TestWeight")] <- NA
			message("the following records were assumed to be a forgotten harv master cycle and will be set to missing. ")
			if(j == 1){
				message("You sent the very first plot straight to the grain tank, didnt you... Good job!")
				print(hm[j,])
			} else if(hm[j-1, c("netWeight")] >= qthresh) {
				print(hm[c(j-1, j),])
				hm[j-1, c("netWeight")] <- NA
			} else {
				print(hm[j,])
			}
			hm[j, c("netWeight", "Moisture", "TestWeight")] <- NA
		}
	}

	if(lbs) hm[["netWeight"]] <- hm[["netWeight"]] * 453.59237
	
	noMoist <- hm$Moisture == 0 | hm$TestWeight == 0
	noMoist[is.na(noMoist)] <- FALSE
	if(any(noMoist)){
		message("The following records did not contain moisture data from the harvest master (usually due to too small sample!)")
		print(hm[noMoist, ])
		message("Setting moisture to average moisture reading for these records to calculate yield...")
		hm[noMoist, c("Moisture", "TestWeight")] <- NA
		avgMoist <- mean(hm$Moisture, na.rm = TRUE)
		hm[noMoist, "Moisture"] <- avgMoist
	}

	hm[["Trial"]] <- gsub("_[^_]+$", "", hm[["plot_name"]])

	hmL <- list()
	for(j in unique(hm[["Trial"]])){
		hmL[[j]] <- hm[hm[["Trial"]] == j,]
	}

	return(hmL)
}
