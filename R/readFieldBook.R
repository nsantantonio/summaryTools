#' readFieldBook function
#'
#' function to (do something)
#'
#' @param path [value]
#' @param year [value]
#' @param testName [value]. Default is NULL
#' @param sheets [value]. Default is NULL
#' @param colOrder [value]. Default is NULL
#' @param scoreTraits [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
readFieldBook <- function(path, year, testName = NULL, sheets = NULL, colOrder = NULL, scoreTraits = NULL){
	require(readxl)
	if(is.null(sheets)) {
		sheets <- excel_sheets(path)
	}
	
	if(is.null(testName)){
		testName <- gsub(".*/|_.*|.xls.*", "", path)
	}

	fbL <- list()
	for(l in sheets) {
		loc <- recodeLoc(l)
		fb <- suppressMessages(as.data.frame(read_excel(path, sheet = l)))
		if(length(fb) == 0){
			next
		}
		emptyCols <- sapply(fb, function(x) all(is.na(x)))
		if(is.null(colOrder)){
			whichCols <- !emptyCols
			colsi <- names(fb)[whichCols]
		} else {
			whichCols <- !{emptyCols & !names(fb) %in% colOrder}
			colsi <- colOrder[colOrder %in% names(fb)]
		}
		fb <- fb[whichCols]
		plotNo <- fb[[grep("^plot$", names(fb), ignore.case = TRUE)]]
		fb[["plot_name"]] <- paste(testName, year, loc, plotNo, sep = "_")
		fbclean <- cleanScores(fb = fb[c("plot_name", colsi)], scoreTraits = scoreTraits)
		fieldNotes <- names(fb)[grep("comment|note", names(fb), ignore.case = TRUE)]
		if(length(fieldNotes)) fbclean[[fieldNotes]] <- fb[[fieldNotes]]
		fbL[[loc]] <- fbclean
	}

	allCols <- Reduce(union, lapply(fbL, names))
	for(i in names(fbL)){
		notin <- !allCols %in% names(fbL[[i]]) 
		for(j in allCols[notin]){
			fbL[[i]][[j]] <- NA
		}
	}
	return(fbL)
}
