#' writeFieldBook function
#'
#' function to (do something)
#'
#' @param testDf [value]
#' @param traits [value]
#' @param keepCols [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
writeFieldBook <- function(testDf, traits, keepCols = NULL){

	if(is.null(keepCols)){
		keepCols <- c("Year", "Location", "Trial", "^plot_name$", "^plot$", "^bloc", "^ent", "^line$", "^pedigree$")
	}
	trtCols <- grep(paste(traits, collapse = "|"), names(testDf))
	traitNames <- names(testDf)[trtCols]

	trtnu <- cleanTraitNames(traitNames)
	notes <- grep("comment|note", names(testDf))
	trtOrder <- NULL
	traitNameUnit <- NULL
	for(i in traits){
		whichNameUnit <- grep(i, traitNames)
		if(length(whichNameUnit)){
			traitNameUnit <- c(traitNameUnit, paste(sapply(trtnu, "[[", whichNameUnit), collapse = " "))
			trtOrder <- c(trtOrder, grep(i, names(testDf)))
		}
	}
	names(testDf)[trtOrder] <- traitNameUnit

	whichKeepCols <- sapply(keepCols, grep, names(testDf), ignore.case = TRUE)
	whichKeepCols <- unlist(whichKeepCols[sapply(whichKeepCols, length) > 0])
	cleanData <- testDf[c(whichKeepCols, trtOrder)]
	return(cleanData)
}
