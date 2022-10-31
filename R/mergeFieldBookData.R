#' mergeFieldData function
#'
#' function to merge fieldbook data collected with the fieldbook application. 
#'
#' @param fb [value]
#' @param dj [value]
#' @param scl [value]
#' @param sqft [value]. Default is 45
#' @param testName [value]. Default is NULL
#' @param year [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
mergeFieldBookData <- function(fb, dj = NULL, scl = NULL, paperfb = NULL, trialDesigns = NULL, trialPassRange = NULL, sqft = 45, testName = NULL, year = NULL, inclPed = TRUE, ...){
	# fb = fbL[[i]]; dj = scldj[["dj"]][[i]]; scl = scldj[["scl"]][[i]]; paperfb = pfbL[[i]]; trialDesigns = trials[grep(paste0("^", i), names(trials))]; trialPassRange = allFields; sqft = 45; testName = i; year = yr; barley = isBarley; inclPed = TRUE
	# fb = fbL[[i]]; dj = scldj[["dj"]][[i]]; scl = scldj[["scl"]][[i]]; paperfb = NULL; trialDesigns = trials[grep(paste0("^", i), names(trials))]; trialPassRange = allFields; sqft = 45; testName = i; year = yr; barley = isBarley; inclPed = TRUE
	if (is.null(testName)) {
		if (!is.null(fb)) testName <- gsub("_.*", "", fb[[1]][["plot_name"]][[1]]) else if (!is.null(scl)) testName <- gsub("_.*", "", scl[[1]][["plot_name"]][[1]]) else testName <- "NoNameTest" 
		message(paste0("No testName provided. Using first record to obtain testName:\n", testName))
	}
	if (is.null(year)) {
		if (!is.null(fb)) year <- as.integer(strsplit(fb[[1]][["plot_name"]][[1]], "_")[[1]][2]) else if (!is.null(scl)) year <- as.integer(strsplit(scl[[1]][["plot_name"]][[1]], "_")[[1]][2]) else as.integer(gsub("-.*", "", Sys.Date()))
		message(paste0("No year provided. Using first record to obtain year:\n", year))
	}

	if (!is.null(fb)){
		names(fb) <- paste0(testName, "_", year, "_", recodeLoc(recodeLoc(names(fb)),locPattern = locCode))
		fb <- lapply(fb, formatFieldBook)
		fb <- lapply(fb, cleanScores)
		trialNames <- names(fb)
	}
	if (!is.null(paperfb)){
		if (is.null(fb)) {
			fb <- paperfb
		} else {
			fbtrials <- union(names(fb), names(paperfb))
			for(j in fbtrials){
				if (!is.null(fb[[j]]) & !is.null(paperfb[[j]])){
					pfbj <- paperfb[[j]][!sapply(paperfb[[j]], function(x) all(is.na(x)))]
					# pfbj[names(pfbj) %in% c("Entry", "Line")
					common <- intersect(names(fb[[j]]), names(pfbj))
					fb[[j]] <- merge(fb[[j]], pfbj, by = c("plot_name", common[common != "plot_name"]), all = TRUE)
				} else if (is.null(fb[[j]])){
					fb[[j]] <- paperfb[[j]][!sapply(paperfb[[j]], function(x) all(is.na(x)))]
				}
			}
		}
	}
	# head(fb[[1]])
	# fb <- fb[names(fb) %in% names(scl)]
	yldL <- list()
	if (!is.null(scl) & !is.null(dj)){		
		names(scl) <- paste0(testName, "_", year, "_", recodeLoc(recodeLoc(names(scl)),locPattern = locCode)) # will accept either BLAVA or blacksburg
		names(dj) <- paste0(testName, "_", year, "_", recodeLoc(recodeLoc(names(dj)),locPattern = locCode)) # will accept either BLAVA or blacksburg
		if (length(names(scl)) != length(names(dj))) {
			message("Scale and Dj lists are different lengths (different no. of trials?)")
			scl <- scl[names(scl) %in% names(dj)]
			dj <- dj[names(dj) %in% names(scl)]
		}

		if (!all(names(scl) == names(dj)) & all(names(scl) %in% names(dj)) & all(names(dj) %in% names(scl))) dj <- dj[names(scl)]
		if (!all(names(scl) == names(dj))) stop("cant merge, scl and dj names dont match!")
		# if (!all(sort(names(fb)) == sort(names(scl))) & all(sort(names(scl)) == sort(names(dj)))) stop("cant merge, fb scl and dj names dont match!")
		if (is.null(fb)) trialNames <- names(scl)
		yieldTrials <- names(scl)

		if (any(!yieldTrials %in% trialNames)) trialNames <- c(trialNames, yieldTrials[!yieldTrials %in% trialNames])
		djMTW <- lapply(dj, function(x) {x[grep("^plot_name|^Moisture|^TestWeight", names(x))]})
		# djMTW <- lapply(dj, "[", c("plot_name", "Moisture", "TestWeight"))
		needConsensusTW <- which(sapply(djMTW, ncol) > 3)
		if (length(needConsensusTW)) {
			for(i in needConsensusTW) {
				djMTW[[i]] <- consensusTW(djMTW[[i]], ...)
				# djMTW[[i]] <- consensusTW(djMTW[[i]])
				# djMTW[[i]] <- consensusTW(djMTW[[i]], barley = barley, defaultDJ = defaultDJ)
			}
		}
		
		sclNW <- lapply(scl, "[", c("plot_name", "netWeight"))

		if (length(sqft) > 1){
			if (length(sqft) != length(trialNames)) stop("either a single value must be provided to 'sqft' or its length must match the number of trials!")
			if (!all(trialNames %in% names(sqft))){
				message("Argument 'sqft' is not named, assuming the 'sqft' coorespond to these trials in order:")
				print(trialNames)
				names(sqft) <- trialNames
			}
		} else {
			sqft <- rep(sqft, length(trialNames))
			names(sqft) <- trialNames
		}
		for(i in yieldTrials){
			scldj <- merge(sclNW[[i]], djMTW[[i]], by = "plot_name", all = TRUE)
			scldj$Yield <- gramsToYield(g = scldj$netWeight, moisture = scldj$Moisture, sqft = sqft[[i]], ...)
			# scldj$Yield <- gramsToYield(g = scldj$netWeight, moisture = scldj$Moisture, sqft = sqft[[i]])
			yldL[[i]] <- scldj
		}
	} else {
		if (is.null(fb)) message(paste0("This trial contains no fieldbook, scale or dickey john data: ", testName))
	}

	if (!is.null(trialDesigns)){
		if (class(trialDesigns) == "trialDesign") {
			trialDesigns <- list(trialDesigns)
			names(trialDesigns) <- unique(trialDesigns[[1]]@trialName)
		} else if (class(trialDesigns) == "data.frame"){
			if (!"trialName" %in% names(trialDesigns)) stop("data.frame of trial designs must have a variable called 'trialName'.")
			trialDesigns <- list(trialDesigns)
			trdfnames <- unique(trialDesigns$trialName)
			if (length(trdfnames) > 1) trialDesigns <- split(trialDesigns[[1]], trialDesigns$trialName)
			names(trialDesigns) <- trdfnames
		}
		trialClass <- unique(sapply(trialDesigns, class)) 
		if (length(trialClass) > 1) stop("trials must all be of the class 'trialsDesign' or 'data.frame'")
		if (trialClass == "trialDesign"){
			td <- lapply(trialDesigns, trialDesignToDataFrame, inclPed = inclPed)
			td <- do.call(rbind, td)
		} else {
			dfNames <- c("plot_name", "trialName", "test", "Year", "Location", "Line", "Entry", "Block", "Plot")
			if (inclPed) c(dfNames, "Pedigree")
			if (length(unique(sapply(trialDesigns, names))) > 1) stop("All trial designs must have the same columns")
			td <- do.call(rbind, trialDesigns)
			if (!all(names(trialDesigns) %in% dfNames)) stop(paste0("All trialDesign data.frames must have columns with names: ", paste(dfNames, collapse = ", ")))
		}
		tdExists <- TRUE
	} else {
		tdExists <- FALSE
	}

	if (!is.null(fb) | !is.null(scl)){

		allDataL <- list()
		for(i in trialNames){
			if (!is.null(yldL[[i]]) & !is.null(fb[[i]])) {
				if (!any(yldL[[i]][["plot_name"]] %in% fb[[i]][["plot_name"]])){
					message(paste0("WARNING! plot names dont match for ", i))
				}
				allDataL[[i]] <- merge(fb[[i]], yldL[[i]], by = "plot_name", all = TRUE)
			}else if (is.null(fb[[i]])) {
				allDataL[[i]] <- yldL[[i]]
			} else {
				allDataL[[i]] <- fb[[i]]
			}
		}
		# need to add cols for tw and moisture from harv m (does this still need done?)
		allVars <- Reduce(union, lapply(allDataL, names))
		for(i in names(allDataL)){
			for(j in allVars){
				if (!j %in% names(allDataL[[i]])) allDataL[[i]][[j]] <- NA
			}
		}
		alldata <- do.call(rbind, allDataL)
	# head(alldata)
	} else {
		# alldata <- data.frame() # I dont know what this was supposed to do
		alldata <- data.frame()
	}

	if (tdExists){
		alldata <- alldata[!names(alldata) %in% c("Entry", "Line", "Plot", "Block", "Pedigree")]
		mcols <- intersect(names(td), names(alldata))
		mcols <- c("plot_name", mcols[!mcols %in% "plot_name"])
		# mcols[!mcols %in% c("Entry", "Line"]
		alldata <- merge(td, alldata, by = mcols, all = TRUE)
	} else {
		alldata$Location <- rep(gsub(".*_", "", names(allDataL)), times = sapply(allDataL, nrow))
		alldata$Year <- year 
		alldata$Plot <- as.integer(gsub(".*_", "", alldata$plot_name))
	}
	# alldata$Trial <- paste(testName, year, Location, sep = "_")
	alldata$Trial <- gsub("_[^_]+$", "", alldata$plot_name)

	isMiss <- is.na(alldata$Yield) | is.na(alldata$TestWeight)
	if (any(isMiss)){
		if (sum(isMiss) > 12){
			message(paste0("There are ", sum(isMiss), " records missing Yield or TestWeight for test: ", testName, "\n"))
		} else {
			message("The following records are missing Yield or TestWeight:\n")
			print(alldata[isMiss, c("plot_name", "Yield", "TestWeight")])
		}
	}

	if (!is.null(trialPassRange) & tdExists){
		if(!all(c("plot_name", "pass", "range") %in% names(trialPassRange))) stop("trialPassRange must contain the variables 'plot_name', 'pass' and 'range'.")
		alldata <- merge(alldata, trialPassRange[c("plot_name", "pass", "range")], by = "plot_name", all.x = TRUE)
		alldata <- alldata[order(alldata$plot_name), ]
		tprExists <- TRUE
	} else if ((!is.null(trialPassRange) & !tdExists)){
		stop("Trial designs must be included if using 'trialPassRange'!")
	} else {
		tprExists <- FALSE
	}

	# names(alldata)[names(alldata) %in% ] # ??? dont remember what this was intended for
	if (!is.null(fb)) {
		nRec <- sum(sapply(fb, nrow))
		if (nrow(alldata) != nRec){
			message(paste0("Compiled data (nrow = ", nrow(alldata), ") does not match number of rows in field book (nrow = ", nRec, ") for ", testName, "!\nCheck for merge errors for ", testName))
		}
	}

	# browser()
	# print(grep("^plot$", names(alldata), ignore.case = TRUE))
	alldata <- alldata[order(alldata$Trial, alldata[[grep("^plot$", names(alldata), ignore.case = TRUE)]]), ]
	if(any(duplicated(alldata$plot_name))){
		message(paste0("The following plots have duplicate records! Check that the bag wasnt weighed twice, etc."))
		alldata[alldata$plot_name %in% alldata$plot_name[duplicated(alldata$plot_name)],]
	}

	# head(alldata)
	return(alldata)
}
