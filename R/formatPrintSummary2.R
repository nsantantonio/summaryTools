#' formatPrintSummary function
#'
#' function to sort summaries. 
#'
#' @param smry summary output from oneYearOneLocSummary, oneYearOverLocSummary, or multiYearSummary
#' @param infoCols list of col names to be included in printed list as is
#' @param tex should a latex table be printed to stdout?
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
formatPrintSummary2 <- function(smry, infoCols = "Line", tex = TRUE, sortBy = NULL, loHi = FALSE, decimals = c(3, 4)){
	# smry = sumipm; infoCols = c("Line", "Entry"); tex = FALSE; sortBy = NULL; loHi = FALSE; decimals = c(3,4)
	formatDecimals <- function(x){
		if((x %% 1) != 0){
			nchar(strsplit(gsub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]])
		}
	}

	avg <- grep("^mean$", smry$Line, ignore.case = TRUE)
	CV <- grep("^C\\.*V$", smry$Line, ignore.case = TRUE)
	LSD <- grep("^L\\.*S\\.*D$", smry$Line, ignore.case = TRUE)
	statRows <- sort(c(avg, CV, LSD))
	if(tex) require(xtable)
	info <- as.matrix(smry[infoCols])
	traits <- grep("_sig", names(smry)) - 1
	sig <- grep("_sig", names(smry))
	otherCols <- 1:length(smry)
	otherCols <- otherCols[!otherCols %in% c(traits, sig, which(names(smry) %in% infoCols))]
	sigpr <- as.matrix(smry[sig])
	sigpr[is.na(sigpr)] <- ""

	

	# sapply(smry[-statRows, traits], function(x) all(x%%1==0))
	# as.matrix(sapply(smry[-statRows, traits], sprintf, fmt = paste0("%#.2f")))
	
	# traitpr <- as.matrix(sapply(smry[-statRows, traits], sprintf, fmt = paste0("%.", decimals[1], "g")))
	# statpr <- as.matrix(sapply(smry[statRows, traits], sprintf, fmt = paste0("%.", decimals[2], "g"))


	# traitpr[traitpr == "NA"] <- ""
	# otherpr <- as.matrix(smry[otherCols])
	# prmat <- matrix(paste0(traitpr, sigpr), nrow = nrow(traitpr), ncol = ncol(traitpr), dimnames = dimnames(traitpr))
	# prmat

	# if(!is.null(infoCols)) prmat <- cbind(info, prmat, otherpr)
	# if(!is.null(sortBy)) {
	# 	if(loHi) {
	# 		prmat <- prmat[order(prmat[[sortBy]]),]
	# 	}  else {
	# 		prmat <- prmat[order(-1L * prmat[[sortBy]]),]
	# 	}
	# }
	# if(tex)  print(xtable(prmat), include.rownames = FALSE)  else return(prmat)
}
