#' formatPrintSummary function
#'
#' function to sort summaries. 
#'
#' @param smry summary output from oneYearOneLocSummary, oneYearOverLocSummary, or multiYearSummary
#' @param infoCols list of col names to be included in printed list as is
#' @param traits should a latex table be printed to stdout?
#' @param tex should a latex table be printed to stdout?
#' @param longtable should a longtable be produced (multi page table)
#' @param digits integer. vector of number of digits per traits, length must equal length(traits)
#' @param type type of summary to be written. valid arguments are 'BLUE' or 'BLUP'
#' @param con path to file connection to write lines to
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
formatPrintSummary <- function(smry, infoCols = "Line", traits = NULL, tex = TRUE, longtable = FALSE, digits = NULL, type = "BLUE", con = NULL, ...){
	# smry = smry[!names(smry) %in% c("Entry")]; infoCols = c("Line"); tex = TRUE; digits = NULL
	# infoCols = c("Line"); tex = FALSE; digits = 2; type = "BLUE"
	# if(tex) require(xtable)

	# need to fix to allow rounding of traits without sig columns!!!
	if(!is.data.frame(smry) & is.list(smry)){
		if (all(names(smry) %in% c("BLUE", "BLUP"))) {
			smry <- smry[[type]]
		}
	}
	if(!is.null(traits)){
		smry <- smry[matchTraits(names(smry), c(infoCols, traits, paste0(traits, "_sig")), warnMultiple = FALSE)]
	}
	if(any(grepl("\\\n", names(smry)))) names(smry) <- gsub("\\\n", " ", names(smry))
	info <- as.matrix(smry[infoCols])
	traits <- grep("_sig", names(smry)) - 1
	sig <- grep("_sig", names(smry))
	if(length(sig) == 0){
		warning("typically you want to run me thorugh addPlusMinus first")
	}

	otherCols <- 1:length(smry)
	otherCols <- otherCols[!otherCols %in% c(traits, sig, which(names(smry) %in% infoCols))]
	sigpr <- as.matrix(smry[sig])
	sigpr[is.na(sigpr)] <- ""
	if(is.null(digits)){
		traitpr <- as.matrix(sapply(smry[,traits], sprintf, fmt = "%.3g"))
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
			# longxtable(prmat, con = con, ...)
		} else {
			xtable2(prmat, ...)
			# xtable2(prmat, con = con, ...)
		}
	} else if(grepl("\\.csv$", con)) {
		write.csv(prmat, file = con, row.names = FALSE, ...)
	} else if(!is.null(con)){
		write.table(prmat, file = con, row.names = FALSE, ...)
	} else {
		return(prmat)
	}
}

