#' formatPrintSummary function
#'
#' function to sort summaries. 
#'
#' @param smry summary output from oneYearOneLocSummary, oneYearOverLocSummary, or multiYearSummary
#' @param infoCols list of col names to be included in printed list as is
#' @param tex should a latex table be printed to stdout?
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
formatPrintSummary <- function(smry, infoCols = "Line", tex = TRUE, longtable = FALSE, digits = NULL, ...){
	# smry = smry[!names(smry) %in% c("Entry")]; infoCols = c("Line"); tex = TRUE; digits = dgs
	# infoCols = c("Line"); tex = FALSE; digits = 2
	# if(tex) require(xtable)
	info <- as.matrix(smry[infoCols])
	traits <- grep("_sig", names(smry)) - 1
	sig <- grep("_sig", names(smry))
	otherCols <- 1:length(smry)
	otherCols <- otherCols[!otherCols %in% c(traits, sig, which(names(smry) %in% infoCols))]
	sigpr <- as.matrix(smry[sig])
	sigpr[is.na(sigpr)] <- ""
	if(is.null(digits)){
		traitpr <- as.matrix(sapply(smry[,traits], sprintf, fmt = "%.4g"))
	} else {
		if(length(digits) == 1) {
			digits <- rep(digits, length(traits))
		} else if(length(digits) != length(traits)) {
			print(c(ndigits = length(digits), ncols = length(traits)))
			stop("length(digits) must equal length of traits!")
		}
		traitpr <- matrix(NA, nrow = nrow(smry), ncol = length(traits), dimnames = list(NULL, names(smry)[traits]))
		for(i in 1:length(traits)){
			traitpr[,names(smry)[traits[i]]] <- sprintf(smry[,traits[i]], fmt = paste0("%.", digits[i], "f"))
		}
	}
	traitpr[traitpr == "NA"] <- ""
	otherpr <- as.matrix(smry[otherCols])
	otherpr[otherpr == "NA"] <- ""
	info[is.na(info)] <- ""
	prmat <- matrix(paste0(traitpr, sigpr), nrow = nrow(traitpr), ncol = ncol(traitpr), dimnames = dimnames(traitpr))
	if(!is.null(infoCols)) prmat <- cbind(info, prmat, otherpr)
	# xtable2(prmat, con = "temp.tex")
	if(tex) {
		if(longtable){
			longxtable(prmat, ...)
		} else {
			xtable2(prmat, ...)
		}
	} else {
		return(prmat)
	}
}

