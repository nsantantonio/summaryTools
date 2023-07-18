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
	breakName <- function(n, breakThreshhold = 4){
		# n = "BYDVScore"; breakThreshhold = 4
		len <- nchar(n)
		len[is.na(len)] <- 0
		if(len > 0){
			if(len > breakThreshhold){
				caps <- gregexpr("[A-Z]", n)
				whichCap <- caps[[1]][which.min(abs(caps[[1]]-ceiling(len/2)))]
			} else {
				whichCap <- 1
			} 
			if(whichCap == 1) {
				name1 <- ""
			} else {
				name1 <- substr(n, 1, whichCap-1)
			}
			name2 <- substr(n, whichCap, len)
			name <- c(name1, name2)
		} else {
			name <- c("", "")
		}
	}
	# x = colnames(tab)
	trtunit <- strsplit(x, unitSep)
	trt <- sapply(trtunit, "[", 1)
	unit <- sapply(trtunit, "[", 2)
	unit[is.na(unit)] <- ""
	trtNameLen <- nchar(trt)
	name <- sapply(trt, breakName)
	if(all(name[1,] == "")){
		name <- name[2,]
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