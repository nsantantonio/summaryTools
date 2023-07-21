#' cleanDJ function
#'
#' function to (do something)
#'
#' @param djdf [value]
#' @param year [value]
#' @param testName [value]. Default is NULL
#' @param minTW [value]. Default is 56
#' @param maxTW [value]. Default is 63
#' @param moistTooHighNA [value]. Default is TRUE
#' @param outsideTWrangeNA [value]. Default is FALSE
#' @param rmChars [value]. Should plot numbers with characters [A-z] be removed? Default is FALSE
#' @param barley [value]. Default is FALSE
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
cleanDJ <- function(djdf, year, testName = NULL, minTW = 55, maxTW = 64, moistThresh = 16, moistTooHighNA = TRUE, outsideTWrangeNA = FALSE, rmChars = FALSE, barley = FALSE){
	# djdf = djij; testName = i; year = yr; minTW = 55; maxTW = 64; moistThresh = 16; moistTooHighNA = TRUE; outsideTWrangeNA = FALSE; barley = FALSE
	if(barley){
		if(minTW == 55){
			minTW <- 40
		}
	}
	if(any(names(djij) %in% c("Sample", "Temp", "User"))){
		names(f)[grep("Sample", names(f))] <- "Sample.ID"
		names(f)[grep("Temp", names(f))] <- "Temperature"
		names(f)[grep("User", names(f))] <- "User.ID"
		names(f) <- gsub(" |/", ".", names(f))
	}
	if(is.null(testName)) {
		message("No testName provided. Using first record to obtain testName.")
		testName <- gsub("_.*", "", djdf[["Sample.ID"]][[1]])
	}
	if(is.null(year)) {
		message("No year provided. Using first record to obtain year.")
		year <- as.integer(strsplit(djdf[["Sample.ID"]][[1]], "_")[[1]][2])
	}

	djdf <- djdf[grep(testName, djdf$Sample.ID),]
	if(nrow(djdf) == 0) {
		stop("No records found for the testName provided!")
	}
	if(length(djdf) > 9) {
		djdf[[9]] <- paste0(djdf[[9]], djdf[[10]])
	}
	if(any(grepl(",", djdf[[9]]))){
		djdf[[9]] <- gsub(",", "", djdf[[9]])
	}
	djdf$Date.Time <- formatTime(djdf$Date.Time, year)
	djdf <- unique(djdf)

	djdf$Moisture[is.na(djdf$Moisture)] <- 0
	if(any(djdf$Moisture == 0) | any(djdf$Moisture > 25)){
		message("The following records have bad moisture readings and will be dropped:")
		print(djdf[djdf$Moisture <= 0 | djdf$Moisture >= 25, ])
		djdf <- djdf[djdf$Moisture > 0 & djdf$Moisture < 25, ]
	}

	dup1 <- which(duplicated(djdf$Sample.ID))
	dup2 <- sort({nrow(djdf):1}[which(duplicated(rev(djdf$Sample.ID)))])
	# dups <- c(dup1, dup2)
	dups <- sort(c(dup1, dup2))

	fixedTWforHighMoist <- NULL
	if(length(dups)){
		djuniq <- djdf[-dups, ]
		djdup <- djdf[dups,]
		latestRec <- list()
		for(j in unique(djdup$Sample.ID)){
			djdupj <- djdup[djdup$Sample.ID == j,]
			latestRec[[j]] <- djdupj[which(djdupj$Date.Time == max(djdupj$Date.Time)),]
			# djdupj$Date.Time[1] <- djdupj$Date.Time[2] 
			if(nrow(latestRec[[j]]) > 1){
				message("\nMore than one record with the latest time. Selecting record with better Moisture and Test weight")
				print(latestRec[[j]][, c("Sample.ID", "Moisture", "Weight", "Date.Time")])
				keep <- which(latestRec[[j]]$Moisture < moistThresh & latestRec[[j]]$Weight >= minTW & latestRec[[j]]$Weight <= maxTW)
				if(length(keep) > 1) {
					message("\nBoth records meet minTW or maxTW requirements. Selecting first record...")
					latestRec[[j]] <- latestRec[[j]][1, ]
				} else if(length(keep) == 0){
					message("\nNeither record meets minTW or maxTW requirements. Please change requirements and rerun.")
					latestRec[[j]] <- latestRec[[j]][rep(FALSE, nrow(latestRec[[j]])),]
				} else {
					cat("Keeping:\n\n")
					print(latestRec[[j]][keep, c("Sample.ID", "Moisture", "Weight", "Date.Time")])
					latestRec[[j]] <- latestRec[[j]][keep, ]
				}
			}
			if(any(djdupj$Moisture > moistThresh)){
				if(nrow(djdupj) > 2){
					message("\nMore than two records for a plot with high moisture. The latest record was was used for test weight, please check that this is correct.")
					print(djdupj[c("Sample.ID", "Moisture", "Weight", "Date.Time")])
				}
				message("\nMoisture too high for first run! keeping moisture reading for yield calc, replacing test weight\n the following is the corrected record:\n")
				mrec <- djdupj[djdupj$Moisture > moistThresh,]
				if(nrow(mrec) > 1) {
					message(paste0("More than one duplicate record with moisture over ", moistThresh, "%! Using latest record with high moisture."))
					mrec <- mrec[which(mrec$Date.Time == max(mrec$Date.Time)),][1,]	
				}
				# mrec[, "Weight"] <- latestRec[[j]][["Weight"]]
				mrec[, "Weight"] <- unique(latestRec[[j]][["Weight"]])
				print(mrec[c("Sample.ID", "Moisture", "Weight", "Date.Time")])
				fixedTWforHighMoist <- c(fixedTWforHighMoist, mrec[["Sample.ID"]])
				latestRec[[j]] <- mrec
			}
		}
		djclean <- rbind(djuniq, do.call(rbind, latestRec))
	} else {
		djclean <- djdf
	}
	row.names(djclean) <- NULL

	highMoistureNotCorrected <- djclean$Moisture >= moistThresh & !djclean$Sample.ID %in% fixedTWforHighMoist
	if(any(highMoistureNotCorrected)){
		message(paste0("\nThe following records have a moisture > ", moistThresh, "% without a second record to correct TW:"))
		print(djclean[highMoistureNotCorrected, c("Sample.ID", "Moisture", "Weight", "Date.Time")])
		if(moistTooHighNA){
			message("Setting Test Weight to missing...")
			djclean[highMoistureNotCorrected, c("Weight")] <- NA
		}
	}

	if(rmChars) {
		plotNos <- gsub(".*_", "", djclean$Sample.ID)
		djclean <- djclean[!grepl("[A-z]", plotNos), ]
	}
	djclean <- djclean[order(as.numeric(gsub(".*_", "", djclean$Sample.ID))),]

	checkPlotNos(gsub(".*_", "", djclean$Sample.ID), testName = testName, type = "Dickey John")
	if(length(unique(djclean$Product)) > 1){
		prodTab <- sort(table(djclean$Product))
		message(paste0("\nMultiple grain types detected! Please check that all samples were run under the correct product type.\nThese records differ from the most common product type: ", names(prodTab[length(prodTab)]), "\n"))
		print(djclean[djclean$Product %in% names(prodTab[-length(prodTab)]), c("Sample.ID", "Moisture", "Weight", "Product")])
	}

	outside <- djclean$Weight < minTW | djclean$Weight > maxTW
	outside[is.na(outside)] <- FALSE
	if(any(outside)){
		message("Test weights outside expectation for test weight! please check input file or adjust minWt and maxWt accordingly.")
		print(djclean[outside,c("Sample.ID", "Moisture", "Weight", "Date.Time")])
		if(outsideTWrangeNA) {
			message("Setting unexpected test weight values to missing and continuing")
			djclean[outside,c("Weight")] <- NA
		}
	}

	names(djclean)[names(djclean) == "Sample.ID"] <- "plot_name"
	names(djclean)[names(djclean) == "Weight"] <- "TestWeight"
	return(djclean)
}
