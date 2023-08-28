#' mergreNotes function
#'
#' function to merge compiled notes with summary tables without losing stats.
#'
#' @param BLUtab summary of line effects with stats at bottom, output from makeBLUtab
#' @param compiled notes across plots for each line, output from compileNotes
#' @return summary table with notes added, and no stats lost.
#' @details [fill in details here]
#' @examples # none
#' @export
mergeNotes <- function(BLUtab, notes){
	# BLUtab <- lsmeansTable; notes <- lineEntNotes
	if("Entry" %in% colnames(BLUtab)) mergeVars <- c("Entry", "Line") else mergeVars <- c("Line")
	murow <- grep("^mean$", BLUtab$Line, ignore.case = TRUE)
	statrows <- murow:nrow(BLUtab)
	BLUtabStats <- BLUtab[murow:nrow(BLUtab),]

	BLUtab$order <- 1:nrow(BLUtab)
	BLUtabNotes <- merge(BLUtab[1:{murow-1},], notes, by = mergeVars, all.x = TRUE)
	BLUtabNotes <- BLUtabNotes[order(BLUtabNotes$order), !names(BLUtabNotes) %in% "order"]
	# BLUtabNotes <- BLUtabNotes[!names(BLUtabNotes) %in% "order"]

	for(i in names(BLUtabNotes)[!names(BLUtabNotes) %in% names(BLUtabStats)]){
		BLUtabStats[[i]] <- NA
	}

	BLUtabNotesStats <- rbind(BLUtabNotes, BLUtabStats[names(BLUtabNotes)])
	rownames(BLUtabNotesStats) <- NULL
	return(BLUtabNotesStats)
}
