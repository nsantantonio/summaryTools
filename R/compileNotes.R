#' compileNotes function
#'
#' function to compile notes taken across plots and trials for each line. 
#'
#' @param dfc data.frame with 
#' @param charTrtCols character vector with column numbers as elements and names(charTrtCols) as names of traits 
#' @return line entry trial and contactenated notes across plots for each 
#' @details [fill in details here]
#' @examples none
#' @export
compileNotes <- function(dfc, charTrtCols){
	# dfc <- dfj
	charTraits <- names(dfc)[charTrtCols]
	allMiss <- sapply(dfc[charTrtCols], function(x) all(is.na(x)))
	charTraits <- charTraits[!allMiss]

	lineCol <- which(names(dfc) == "Line")
	entCol <- which(names(dfc) == "Entry")
	trialCol <- which(names(dfc) == "Trial")

	lineL <- list()
	uniqLines <- unique(dfc$Line)
	uniqLines <- uniqLines[!is.na(uniqLines)]
	for(i in uniqLines){
		dfi <- dfc[dfc$Line %in% i, c(lineCol, entCol, trialCol, charTrtCols)]
		cL <- list()
		for(j in unique(dfi$Trial)){
			dfij <- dfi[dfi$Trial == j, ] 
			trl <- gsub(".*_", "", j)
			for(k in charTraits){
				chstr <- dfij[[k]]
				chstr <- chstr[!is.na(chstr)]
				cL[[k]][[j]] <- if(!all(chstr == "")) paste0(trl, ": ", paste0(chstr, collapse = ",")) else ""
			}
		}

		concatChar <- lapply(cL, function(x) paste(x[x != ""], collapse = "; "))
		lineL[[i]] <- data.frame(unique(dfij[c("Line", "Entry")]), concatChar, check.names = FALSE)
	}
	lineEntChar <- do.call(rbind, lineL)
	lineEntChar <- lineEntChar[order(lineEntChar$Entry),]
	
	return(lineEntChar)
}