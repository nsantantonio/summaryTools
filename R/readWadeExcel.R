#' readWadeExcel function
#'
#' function to read Wade's crazy excel table summaries. 
#'
#' @param path file path
#' @param startLine optional start line
#' @param stopLine optional stop line
#' @param keepLSD should the plus Minus LSD separation from mean be kept?
#' @param tooLong parameter to determine if the first col name is just a title 
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
readWadeExcel <- function(path, startLine = NULL, stopLine = NULL, keepLSD = FALSE, tooLong = 33, ...){
	require(readxl)
	raw <- as.data.frame(read_excel(path, ...))
	# raw <- as.data.frame(read_excel("/home/nsant/Dropbox/releases2022/dataSummaries/VA17W-75/19SW-Data Tables-Final.xlsx", sheet = "OverLocation"))
	missFirst <- is.na(raw[[1]])
	notMiss <- which(!missFirst)
	isMiss <- which(missFirst)
	Line <- grep("Line", raw[[1]])
	avg <- grep("Average", raw[[1]], ignore.case = TRUE)[1]
	LSD <- grep("LSD", raw[[1]], ignore.case = TRUE)
	CV <- grep("C\\.*V", raw[[1]], ignore.case = TRUE)

	statLines <- c(avg, CV, LSD)

	if(is.null(startLine)){
		startLine <- min(notMiss[notMiss > Line])
	}

	if(is.null(stopLine)){
		stopLine <- min(c(avg, isMiss[isMiss > startLine]) - 1)
	}

	dat <- raw[startLine:stopLine, ]
	names(dat)[1] <- "Line"
	hasTitle <- nchar(names(raw))[1] > tooLong

	for(i in 2:length(raw)){
		varName <- raw[[i]][1:{startLine-1}]
		varName[is.na(varName)] <- ""
		if(!hasTitle) varName <- c(i, varName)
		varName <- gsub("\\s+", " ", trimws(paste(varName, collapse = " ")))
		names(dat)[i] <- varName
	}

	# table(unlist(dat[names(dat) == ""]))
	if(any(!unlist(dat[names(dat) == ""]) %in% c("", "+", "-", NA))){
		warning("some information in unnamed columns may be lost!")
		unlist(dat[names(dat) == ""])[!unlist(dat[names(dat) == ""]) %in% c("", "+", "-", NA)]
	}

	for(i in which(names(dat) == "")){
		names(dat)[i] <- paste0(trimws(gsub("\\(.*", "", names(dat)[i-1])), "_sig")
	}

	stat <- raw[statLines,]
	names(stat) <- names(dat)
	stat["Line"] <- c("mean", "CV", "LSD")
	smry <- rbind(dat, stat)

	smry[grep("_sig", names(smry))-1] <- lapply(smry[grep("_sig", names(smry))-1], as.numeric)

	if(!keepLSD){
		smry <- smry[-grep("_sig", names(smry))]
	}
	return(smry)
}
