#' cleanScale function
#'
#' function to (do something)
#'
#' @param scl [value]
#' @param year [value]
#' @param testName [value]. Default is NULL
#' @param tare [value]. Default is NULL
#' @param minWt [value]. Default is 500
#' @param maxWt [value]. Default is 6000
#' @param rmDup [value]. Default is FALSE
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
cleanScale <- function(scl, year, testName = NULL, tare = NULL, minWt = 500, maxWt = 6000, rmDup = FALSE){
	headers <- sapply(c("plot", "weight", "tare"), function(x) {grep(x, names(scl), ignore.case = TRUE)})
	if (!all(headers == 1:3)) {
		stop("Scale file must have first three columns named 'plot', 'weight', and 'tare' in that order (not case sensitive)")
	}
	sclNames  <- c("plot_name", "weight", "tare")
	if(length(scl) > 3) {
	 	if(length(scl) > 4) {
	 		message("Any comments or notes must be in 4th column, all others will be ignored")
	 	}
		sclNames <- c(sclNames, "Notes")
	 }
	names(scl) <- sclNames

	if(is.null(testName)) {
		message("No testName provided. Using first record to obtain testName.")
		testName <- gsub("_.*", "", scl[["plot_name"]][[1]])
	}
	if(is.null(year)) {
		message("No year provided. Using first record to obtain year.")
		year <- as.integer(strsplit(scl[["plot_name"]][[1]], "_")[[1]][2])
	}
	if(any(duplicated(scl$plot_name))){
		message(paste0("\nDuplicated plots detected! Check input file:\n\n", j, "\n"))
		if(rmDup){
			dup1 <- which(duplicated(scl$plot_name))
			dup2 <- sort({nrow(scl):1}[which(duplicated(rev(scl$plot_name)))])
			dups <- c(dup1, dup2)
			uniqScl <- scl[-dups,]
			sclDup <- scl[dups,]
			dupL <- list()
			for(i in unique(sclDup$plot_name)) {
				sclDupi <- sclDup[sclDup$plot_name == i,]
				dupL[[i]] <- sclDupi[which.max(sclDupi$weight), ]
			}
			scl <- rbind(uniqScl, do.call(rbind, dupL))
		}
	}

	if(any(grepl("[A-z]", scl$tare))){
		whichCom <- grep("[A-z]", scl$tare)
		scl[whichCom, "Notes"] <- paste0(scl[whichCom, "tare"], scl[whichCom, "Notes"])
		scl[whichCom, "tare"] <- NA
		scl$tare <- as.numeric(scl$tare)
		message(paste0("Notes detected in tare column. Moving to Notes column..."))
	}

	hasTare <- which(!is.na(scl$tare))
	if(length(hasTare) == 0 & is.null(tare)) {
		stop("No tare provided! Please provide an argument to 'tare' to continue or include a tare column with at least one tare weight in the first record.")
	} else if(length(hasTare)) {
		if(hasTare[1] != 1) {
			scl$tare[1] <- scl$tare[hasTare[1]] # when tare is not enterd in first cell
			hasTare <- c(1, hasTare)
		}
		if(any(hasTare != 1) & length(unique(scl$tare[!is.na(scl$tare)])) > 1){
			message("Multiple tares detected! Each missing tare weight will be assumed to be equal to previous tare weight. If this is unintended, please populate file with all tare weights.")
			for(i in 2:nrow(scl)){
				if(is.na(scl$tare[i])){
					scl$tare[i] <- scl$tare[i-1]
				} 
			}
		} else {
			scl$tare <- scl$tare[hasTare[1]]
		}
	} else {
		scl$tare <- tare
	}
	
	scl$netWeight <- scl$weight - scl$tare
	outside <- scl$netWeight < minWt | scl$netWeight > maxWt
	outside[is.na(outside)] <- FALSE
	if(any(outside)){
		message("Plot weights outside expectation for gram unit weights! please check input file or adjust minWt and maxWt accordingly.")
		print(scl[outside,])
		message("setting gram weight values to missing and continuing")
		scl[outside,c("weight", "netWeight")] <- NA
	}
	sclClean <- scl[order(as.numeric(gsub(".*_", "", scl$plot_name))),]
	# names(sclClean)[names(sclClean) == "plot"] <- "plot_name"
	plotNos <- gsub(".*_", "", sclClean$plot_name)
	checkPlotNos(plotNos, testName = testName, type = "Scale")
	return(sclClean)
}
