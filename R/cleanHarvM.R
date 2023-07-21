#' cleanHarvM function
#'
#' function to (do something)
#'
#' @param hmi [value]
#' @param lbs [value]. Default is TRUE
#' @param forgotCycleThreshold [value]. Default is 0.2
#' @param lowMoistThreshold threshold for too low moisture. records with low moisture will be set to NA. Default is 9
#' @param qnt [value]. Default is 0.99
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
cleanHarvM <- function(hmi, blockName, lbs = TRUE, forgotCycleThreshold = 0.2, lowMoistThreshold = 9, qnt = 0.99, keepCols = NULL, keepSeq = FALSE, rmPlotPattern = NULL){
	# blockName = "TestBlock"; lbs = TRUE; forgotCycleThreshold = 0.2; qnt = 0.99; keepCols = NULL; rmPlotPattern = "_b$|_w$"
	hmi <- hmi[order(hmi[[grep("Sequence", names(hmi))]]),]
	# head(hmi) 
	if("Line" %in% names(hmi)) { # this needs to happen later to correctly deal with bad cycles when they have a fill plot.. 
		hmi <- hmi[!hmi[[grep("Line", names(hmi))]] %in% "fill",]
	}
	cols <- sapply(c("^plot.*id|^id.*1", "^moisture", "^weight", "^test", "note", "sequence$", "^range$", "^row$"), function(x) grep(x, names(hmi), ignore.case = TRUE))
	
	hm <- data.frame(blockName = blockName, hmi[cols])
	# hm <- hmi[cols]
	names(hm) <- c("blockName", "plot_name", "Moisture", "netWeight", "TestWeight", "NotesHM", "Sequence", "RangeHM", "RowHM")
	if(!is.null(rmPlotPattern)){
		hm[grepl(rmPlotPattern, hm$plot_name), c("Moisture", "netWeight", "TestWeight")] <- NA
	}	

	if(!is.null(keepCols)) hm[keepCols] <- hmi[keepCols]
	badCycle <- which(hm[["netWeight"]] < forgotCycleThreshold)
	hm[badCycle,]
	if(length(badCycle)){
		qthresh <- quantile(hm[, "netWeight"], probs = qnt, na.rm = TRUE)

		for (j in badCycle){
			# hm[c(j-1, j), c("netWeight", "Moisture", "TestWeight")] <- NA
			message("the following records were assumed to be a forgotten harv master cycle and will be set to missing. ")
			if(j == 1){
				message("You sent the very first plot straight to the grain tank, didnt you... Good job!")
				print(hm[j,])
			} else if(is.na(hm[j-1, c("netWeight")])) {
				print(hm[c(j-1, j),])
				hm[j-1, c("netWeight")] <- NA  # this fixes trippled up plots. Not a great or general solution.
			} else if(hm[j-1, c("netWeight")] >= qthresh) {
				print(hm[c(j-1, j),])
				hm[j-1, c("netWeight")] <- NA
			} else {
				print(hm[j,])
			}
			hm[j, c("netWeight", "Moisture", "TestWeight")] <- NA
		}
	}

	if(lbs) hm[["netWeight"]] <- hm[["netWeight"]] * 453.59237 # convert lbs to grams
	
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

	lowMoist <- hm$Moisture < lowMoistThreshold
	lowMoist[is.na(lowMoist)] <- FALSE
	if(any(lowMoist)){
		message(paste0("The following records had moisture data recorded below the lowMoistThreshold of ", lowMoistThreshold, " (can be due to too small sample)"))
		print(hm[lowMoist, ])
		message("Setting moisture to average moisture reading for these records to calculate yield...")
		hm[lowMoist, c("Moisture", "TestWeight")] <- NA
		avgMoist <- mean(hm$Moisture, na.rm = TRUE)
		hm[lowMoist, "Moisture"] <- avgMoist
	}
	
	hm[["Trial"]] <- gsub("_[^_]+$", "", hm[["plot_name"]])
	if(!keepSeq) hm <- hm[!names(hm) %in% "Sequence"]

	if(!is.null(rmPlotPattern)){
		rmhm <- hm[grepl(rmPlotPattern, hm$plot_name), ]
		hm <- hm[!grepl(rmPlotPattern, hm$plot_name), ]
		if(nrow(rmhm) > 0){
			message("the following plots were removed because they matched the pattern given to rmPlotPattern")
			print(rmhm$plot_name)
		}
	}	

	hmL <- list()
	for(j in unique(hm[["Trial"]])){
		hmL[[j]] <- hm[hm[["Trial"]] == j,]
	}

	return(hmL)
}
