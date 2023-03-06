#' formatXtabColNames function
#'
#' function to format data.frame names from 'Trait|unit (#trials)' to multi lines for printing in xtable
#'
#' @param x character vector of format 'Trait|unit (#trials)'
#' @param unitSep unit sep (must have escape characters if neede, i.e. '|' would be "\\|")
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
formatXtabColNames <- function(x, unitSep = "\\|"){
	# x = colnames(tab)
	trtunit <- strsplit(x, unitSep)
	trt <- sapply(trtunit, "[", 1)
	unit <- sapply(trtunit, "[", 2)
	unit[is.na(unit)] <- ""
	lastCap <- as.vector(sapply(gregexpr("[A-Z]", trt), tail, 1))
	name1 <- NULL
	name2 <- NULL
	for(i in 1:length(trtunit)){
		name1[i] <- substr(trt[i], 1, lastCap[i]-1)
		name2[i] <- substr(trt[i], lastCap[i], nchar(trt)[i])
	}
	if(all(name1 == "")){
		name <- name2
	} else {
		name <- rbind(name1, name2)
	}
	if(any(grepl("\\(", unit))){
		lastParen <- as.vector(sapply(gregexpr("\\(", unit), tail, 1))
		unit1 <- NULL
		unit2 <- NULL
		for(i in 1:length(trtunit)){
			unit1[i] <- substr(unit[i], 1, lastParen[i]-1)
			unit2[i] <- substr(unit[i], lastParen[i], nchar(unit)[i])
		}
		unit <- rbind(unit1, unit2)
	}
	nameUnit <- rbind(name, unit)
	rownames(nameUnit) <- NULL
	return(nameUnit)
}