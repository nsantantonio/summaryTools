#' mergeFieldData function
#'
#' function to (do something)
#'
#' @param fb [value]
#' @param dj [value]
#' @param scl [value]
#' @param sqft [value]. Default is 45
#' @param testName [value]. Default is NULL
#' @param year [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
mergeFieldData <- function(fb, dj, scl, sqft = 45, testName = NULL, year = NULL, ...){
	if(is.null(testName)) {
		testName <- gsub("_.*", "", fb[[1]][["plot_name"]][[1]])
		message(paste0("No testName provided. Using first record to obtain testName:\n", testName))
	}
	if(is.null(year)) {
		year <- as.integer(strsplit(fb[[1]][["plot_name"]][[1]], "_")[[1]][2])
		message(paste0("No year provided. Using first record to obtain year:\n", year))
	}
	names(fb) <- paste0(testName, "_", year, "_", recodeLoc(names(fb)))
	names(scl) <- paste0(testName, "_", year, "_", recodeLoc(recodeLoc(names(scl)),locPattern = locCode)) # will accept either BLAVA or blacksburg
	names(dj) <- paste0(testName, "_", year, "_", recodeLoc(recodeLoc(names(dj)),locPattern = locCode)) # will accept either BLAVA or blacksburg

	fb <- fb[names(fb) %in% names(scl)]
	if(length(names(scl)) != length(names(dj))) {
		message("Scale and Dj lists are different lengths (different no. of trials?)")
		scl <- scl[names(scl) %in% names(dj)]
		dj <- dj[names(dj) %in% names(scl)]
	}

	# if(!all(names(fb) == names(scl)) & all(names(scl) == names(dj))) stop("cant merge, fb scl and dj names dont match!")
	if(!all(sort(names(fb)) == sort(names(scl))) & all(sort(names(scl)) == sort(names(dj)))) stop("cant merge, fb scl and dj names dont match!")
	trials <- names(fb)
	djMTW <- lapply(dj, "[", c("plot_name", "Moisture", "TestWeight"))
	sclNW <- lapply(scl, "[", c("plot_name", "netWeight"))

	if(length(sqft) > 1){
		if(length(sqft) != length(trials)) stop("either a single value must be provided to 'sqft' or its length must match the number of trials!")
		if(!all(trials %in% names(sqft))){
			message("Argument 'sqft' is not named, assuming the 'sqft' coorespond to these trials in order:")
			print(trials)
			names(sqft) <- trials
		}
	} else {
		sqft <- rep(sqft, length(trials))
		names(sqft) <- trials
	}
	yldL <- list()
	for(i in trials){
		scldj <- merge(sclNW[[i]], djMTW[[i]], by = "plot_name", all = TRUE)
		scldj$Yield <- gramsToYield(g = scldj$netWeight, moisture = scldj$Moisture, sqft = sqft[[i]], ...)
		yldL[[i]] <- scldj
	}

	allDataL <- list()
	for(i in trials){
		if(!any(yldL[[i]][["plot_name"]] %in% fb[[i]][["plot_name"]])){
			message(paste0("WARNING! plot names dont match for ", i))
		}
		fbYld <- merge(fb[[i]], yldL[[i]], by = "plot_name")
		allDataL[[i]] <- fbYld
	}

	alldata <- do.call(rbind, allDataL)
	head(alldata)
	alldata$Location <- rep(gsub(".*_", "", names(allDataL)), times = sapply(allDataL, nrow))
	alldata$Year <- year 
	alldata$Trial <- paste(testName, alldata$Year, alldata$Location, sep = "_")

	isMiss <- is.na(alldata$Yield) | is.na(alldata$TestWeight)
	if(any(isMiss)){
		message("The following records are missing Yield or TestWeight:\n")
		print(alldata[isMiss, c("plot_name", "Yield", "TestWeight")])
	}
	# names(alldata)[names(alldata) %in% ] # ??? dont remember what this was intended for
	nRec <- sum(sapply(fb, nrow))
	if(nrow(alldata) != nRec){
		message(paste0("Compiled data (nrow = ", nrow(alldata), ") does not match number of rows in field book (nrow = ", nRec, ")!\nCheck for merge errors for ", testName))
	}

	# browser()
	# print(grep("^plot$", names(alldata), ignore.case = TRUE))
	alldata <- alldata[order(alldata$Trial, alldata[[grep("^plot$", names(alldata), ignore.case = TRUE)]]), ]
	return(alldata)
}
