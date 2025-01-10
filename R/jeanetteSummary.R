#' jeanetteSummary function
#'
#' function to read Jeanette Lyerly summaries and produce summaries over locations.
#'
#' @param path file path
#' @param nlineHeader number of lines in the header
#' @param checks checks to be printed, only used if inEnv = TRUE
#' @param nWide number of tables wide for inEnv table printing, default is 5
#' @param inEnv logical. should means within environment be printed? default is = FALSE
#' @param keep traits to keep, default is NULL
#' @param ignore traits to ignore, default is NULL. Note hasnt been implemented yet
#' @param printMeans logical. Should he means be printeed? Default is TRUE. Specify a file path to 'con' to write to a file, else tex tables will be printed to stdout.
#' @param byLoc trait for which to prodce tables by location, default is "YLD". 
#' @param shorthand logical. Should shorthand for traits be used? Makes tables tighter. default is TRUE,
#' @param sdigits numeric. vector of number of digits for single environment traits. for multi environment traits, use 'digits' argument whcih is passed to formatPrintSummary. default is null, and no rounding will be accomplished
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
jeanetteSummary <- function(path, checks, nWide = 5, inEnv = FALSE, keep = NULL, ignore = NULL, printMeans = TRUE, byLoc = "YLD_BUPA", shorthand = TRUE, sdigits =  NULL, ...) {
	# require(summaryTools); path <- "/home/nsant/Dropbox/releases2024/analysis/2023_UESRWN_Prelim_Data (03Aug23).xlsx"; shorthand = TRUE; nWide = 5; byLoc = "YLD_BUPA"
	# require(summaryTools); path <- "/home/nsant/Dropbox/releases2024/analysis/MDXNsummary2021.xlsx"; shorthand = TRUE; nWide = 5; byLoc = "YLD_BUPA"; keep = NULL; ignore = c("LDG", "PM", "wst");
	# require(summaryTools); path = "/home/nsant/Dropbox/releases2024/analysis/20gawn-Data Running Means.xlsx"; shorthand = TRUE; nWide = 5; byLoc = "YLD_BUPA"; keep = NULL; ignore = c("LDG", "PM", "wst"); sdigits = c(0,1,1,1)
	# require(summaryTools); path = "/home/nsant/Dropbox/releases2024/analysis/UESRWWN2021_Preliminary.xlsx"; shorthand = TRUE; nWide = 5; byLoc = "YLD_BUPA"; keep = NULL; ignore = c("LDG", "PM", "wst"); sdigits = c(0,1,1,1)
	
	require(readxl)
	require(lsmeans)
	lookup <- function(x, lookupList){
		for(i in names(lookupList)){
			x[x %in% lookupList[[i]]] <- i
		}
		return(x)
	}
	makeLong <- function(dat, startLoc, trait){
		datL <- list()
		for(i in names(dat)[startLoc:length(dat)]) {
			# N <- grep("mean", dat$Line, ignore.case = TRUE) - 1
			# dati <- dat[1:N, c("Entry", "Line", i)]
			dati <- dat[c("Entry", "Line", i)]
			dati$Location <- i
			names(dati)[3] <- trait
			datL[[i]] <- dati
		}
		datLong <- do.call(rbind, datL)
	}
	canBeNumeric <- function(x) { # taken from https://stackoverflow.com/questions/24129124/how-to-determine-if-a-character-vector-is-a-valid-numeric-or-integer-vector
	    stopifnot(is.atomic(x) | is.list(x)) # check if x is a vector
	    numNAs <- sum(is.na(x))
	    newNAs <- suppressWarnings(sum(is.na(as.numeric(x))))
	    return(newNAs == numNAs)
	}
	# keep = c("YLD", "TW", "HD", "HT", )
	# this will fail if you change to data.frame, as it will add a .[0-9] integer to repeated traits (i.e. merged col headers in excel)
	# functions correctly if it is a data tbl from the readxl package, at least for now. If this breaks, thats probably why
	smry <- read_excel(path, na = c("", ".", "-", "NA"))
	for(i in 2:length(smry)) { 
		if(grepl("^\\.{3}[0-9]*", names(smry)[i])){
			# print(i)
			names(smry)[i] <- names(smry)[i-1]
		}
	}
	names(smry) <- gsub("\\.{3}[0-9]*", "", names(smry)) # this doesnt make sense why I had this here! the loop below relies on the ...[0-9] to assign previous name
	# matrix(unique(unique(names(smry))))
	hasData <- !sapply(smry, function(x) all(is.na(x)))
	smry <- smry[hasData]
	if (!is.null(keep)) {
		smry <- smry[grepl(paste0(c("Line", "Trial", "Entry", "Pedigree", "DESIG", "EXPT", "YEAR", "ENTRY", "PED", keep), collapse = "|"), names(smry))]
	} else if (!is.null(ignore)) {
		smry <- smry[!grepl(paste0(ignore, collapse = "|"), names(smry))]
	}
	# names(smry) <- gsub("(.*)_", "\\1|", names(smry))
	# names(smry) <- gsub("([A-Z]*)09", "\\1|0-9", names(smry))

	renameCols <- NULL
	renameCols[["Line"]] <- c("DESIG")
	renameCols[["Test"]] <- c("EXPT")
	renameCols[["Year"]] <- c("YEAR")
	renameCols[["Entry"]] <- c("ENTRY")
	# renameCols[["ID"]] <- c(
	renameCols[["Line"]] <- c("DESIG") 
	renameCols[["Pedigree"]] <- c("PED") 
	renameCols[["GrainYield|bu/ac"]] <- c("GrainYield", "YLD_BUPA", "Yield", "YIELD") 
	renameCols[["TestWeight|lb/bu"]] <- c("TestWeight", "TW_LBBU", "TWT", "TW") 
	renameCols[["HeadingDate|Julian"]] <- c("HeadingDate", "HD_JULIAN", "HD") 
	renameCols[["Height|in"]] <- c("Height", "HT_IN", "Height", "Ht", "HT") 
	renameCols[["Lodging|0-9"]] <- c("Lodging", "LOD09", "LDG") 
	renameCols[["PowderyMildew|0-9"]] <- c("PowderyMildew", "PMD09", "PM") 
	renameCols[["BeautyScore|0-9"]] <- c("BeautyScore", "PHE09") # Kkkkkkk!
	renameCols[["WinterSurvival|%"]] <- c("WinterSurvival", "WINSUR") 
	renameCols[["WinterHardiness|0-9"]] <- c("WinterHardiness", "WINDM09") 
	renameCols[["Other|chr"]] <- c("Other", "OTHER") 
	renameCols[["LeafRust|0-9"]] <- c("LeafRust", "LFRUST09", "LR", "LR09") 
	renameCols[["StripeRust|0-9"]] <- c("StripeRust", "STRIPE09", "YR", "YR09") 
	renameCols[["ST_LF|0-9"]] <- c("ST_LF", "ST_LFB09") # septoria on leaf? why no sept head?
	renameCols[["StemRust|0-9"]] <- c("StemRust", "STEMRUST09") 
	if(shorthand){
		renameCols[["SBMV|0-9"]] <- c("SBMV", "SBMV09") 
		renameCols[["BYDV|0-9"]] <- c("BYDV", "BYDV09") 
		renameCols[["HF|0-9"]] <- c("HF", "HFLY09") 
		renameCols[["FHB|0-9"]] <- c("FHB", "FHB09") 
		renameCols[["FHBSev|%"]] <- c("FHBSev", "FHBSEV") # may need to change this to 0-9?
		renameCols[["FDK|%"]] <- c("FDK", "FHBFDK") 
	} else {
		renameCols[["SoilBorneMosaicVirus|0-9"]] <- c("SoilBorneMosaicVirus", "SBMV09") 
		renameCols[["BarleyYellowDwarfVirus|0-9"]] <- c("BarleyYellowDwarfVirus", "BYDV09") 
		renameCols[["HessianFly|0-9"]] <- c("HessianFly", "HFLY09") 
		renameCols[["FHBPlantResponse|0-9"]] <- c("FHBPlantResponse", "FHB09") 
		renameCols[["FHBSeverity|%"]] <- c("FHBSeverity", "FHBSEV") # may need to change this to 0-9?
		renameCols[["FusariumDiseasedKernels|%"]] <- c("FusariumDiseasedKernels", "FHBFDK") 

	}
	renameCols[["DON|ppm"]] <- c("DON", "FHBDON") 
	renameCols[["FallingNumber|sec"]] <- c("FallingNumber", "FALLNUM") # unit?
	renameCols[["Protein|%"]] <- c("Protein", "PROTEIN") 
	renameCols[["KernelHardness|"]] <- c("KernelHardness", "KHARDNESS") # unit?
	renameCols[["KernelDiameter|mm"]] <- c("KernelDiameter", "KDIAMETER") # unit?
	renameCols[["KernelWeight|g"]] <- c("KernelWeight", "KWEIGHT") # thousand kernel weight?
	renameCols[["WaterAbsorption"]] <- c("WaterAbsorption", "WATERABS") 
	renameCols[["DevTime|"]] <- c("DevTime", "DEVTIME") # no idea
	renameCols[["Stability|"]] <- c("Stability", "STABILITY") # no idea what this is 
	renameCols[["GlutenIndex|"]] <- c("GlutenIndex", "GLUINDEX") #units?
	renameCols[["WetGluten|"]] <- c("WetGluten", "WETGLU") #units?
	renameCols[["DryGluten|"]] <- c("DryGluten", "DRYGLU") #units?
	renameCols[["WBWG|"]] <- c("WBWG", "WBWG") # no idea
	# renameCols[["FHB09_MN"]] <- 
	# renameCols[["FHBSEV_MN"]]
	# renameCols[["FHBFDK_MN"]]
	# renameCols[["FHBDON_MN"]]
	# WHat about awns? septoria, others?

	names(smry) <- lookup(names(smry), renameCols)

	uniqCols <- unique(names(smry))
	namesSum <- names(smry)
	multiEnvTrait <- unique(names(smry)[duplicated(names(smry))])
	
	singleEnvTrait <- uniqCols[!uniqCols %in% c("Year", "Test", "Line", "Trial", "Entry", "Pedigree", "pedigree", "DESIG", "EXPT", "YEAR", "ENTRY", "PED", ignore, multiEnvTrait)]

	whichMean <- grep("^mean$", smry$Line, ignore.case = TRUE)
	whichCV <- grep("^CV", smry$Line, ignore.case = TRUE)
	whichLSD <- grep("^LSD", smry$Line, ignore.case = TRUE)
	meanLSDCV <- c(whichMean, whichLSD, whichCV)
	
	# may need to allow for no CV or LSD, etc
	smry$Line[meanLSDCV] <- c("mean", "LSD", "CV")
	
	lineEnd <- min(meanLSDCV) - 1
	lineStart <- min(which(!is.na(smry$Line)))

	if("ID" %in% names(smry)){
		if(!all(smry$ID[lineStart:lineEnd] == smry$Line[lineStart:lineEnd])){
			warning("ID and DESIG differ. Using DESIG as the line name. This may produce unwanted results")
		}
	}

	nEnt <- lineEnd - lineStart +1
	yr <- unique(smry$Year[lineStart:lineEnd])
	if(yr < 2000) yr <- yr+2000
	Test <- unique(smry$Test[lineStart:lineEnd])
	TestYear <- paste0(Test, "_", yr)
	
	if(any(smry[whichLSD,] %in% "NS")) {
		isNS <- unlist(as.data.frame(smry[whichLSD,])) %in% "NS"
		smry[whichLSD, isNS] <- NA # NS is not significant
	}
	menvL <- list()
	locL <- list()
	for(i in multiEnvTrait){
		# i = multiEnvTrait[[1]]
		# dfli <- smry[c("Entry", "Line", grep(paste0("^", gsub("\\|.*", "", i)), names(smry)))]
		
		dfli <- smry[names(smry) %in% c("Entry", "Line", i)]
		locs <- unlist(dfli[1,3:length(dfli)])
		names(dfli) <- c("Entry", "Line", locs)
		locs <- locs[!locs %in% c("AVG", "RNK")]
		if(any(is.na(locs))) stop(paste0("all traits must have a location listed. The following trait does not: ", i))
		dfli <- as.data.frame(dfli[lineStart:max(meanLSDCV), c("Entry", "Line", locs)])
		for(j in locs){
			if(canBeNumeric(dfli[[j]])) dfli[[j]] <- as.numeric(dfli[[j]])
		}
		locL[[i]] <- as.data.frame(dfli[c(1:{lineEnd - lineStart + 1}, meanLSDCV - lineStart + 1), c("Entry", "Line", locs)])
		menvL[[i]] <- makeLong(dfli[1:{lineEnd-1}, ], 3, i)
	}

	singleEnvDf <- as.data.frame(smry[c(lineStart:lineEnd, meanLSDCV), singleEnvTrait])
	if(!is.null(sdigits)) {
		if(length(singleEnvDf) != length(singleEnvDf)){
			stop(paste0("Length of sdigits must equal number of single environment traits!\n", "sdigits = ", length(singleEnvDf), ", ntraits = ", length(singleEnvDf)))
		}
		for(i in 1:length(singleEnvDf)){
			if(canBeNumeric(singleEnvDf[[i]])) singleEnvDf[[i]] <- as.numeric(singleEnvDf[[i]])
			singleEnvDf[[i]] <- round(singleEnvDf[[i]], sdigits[i])
		}
	}

	menvLong <- Reduce(function(...) merge(..., all = TRUE), menvL)
	menvLong$Trial <- paste0(TestYear, "_", menvLong$Location)
	menvLong$plot_name <- paste0(menvLong$Trial, "_", menvLong$Entry)
	menvLong$Block <- 1L

	menvsum <- oneYearOverLocSummary(menvLong, multiEnvTrait)
	metMeans <- addTraitPercRank(sortSummary(menvsum$BLUE, trait = "GrainYield"), "GrainYield", ties.method = "first")
	metMeans <- addPlusMinus(metMeans, traits = multiEnvTrait)
	# formatPrintSummary(usnMeans, infoCols = c("Line", "YieldPercMean", "YieldRank"), include.rownames = FALSE)
	infoVars <- names(metMeans)[grep("^Line$|PercMean$|Rank$", names(metMeans))]
	# if(length(singleEnvTrait)) infoVars <- c(infoVars, singleEnvTrait)
	# metMeans

	if(length(singleEnvTrait)) metMeans <- cbind(metMeans, singleEnvDf)
	# formatPrintSummary(metMeans, infoCols = infoVars, include.rownames = FALSE, digits = c(0,1,0,0,1), con = "test.tex", longtable = TRUE, caption="Performance ")
	if(printMeans) formatPrintSummary(metMeans, infoCols = infoVars, include.rownames = FALSE, ...)

	rL <- metMeans
	######
	if(inEnv){

		inEnvTrt <- lookup(byLoc, renameCols)
		# locL[["GrainYield|bu/ac"]][["OverLocations"]] <- menvsum$BLUE[[grep("GrainYield\\|bu\\/ac\\s\\(", names(menvsum$BLUE))]]
		locL[[inEnvTrt]][["OverLocations"]] <- menvsum$BLUE[[grep(gsub("\\|.*", "\\\\|", inEnvTrt), names(menvsum$BLUE))]]

		names(menvL)
		yldLoc <- sortSummary(locL[[inEnvTrt]], "OverLocations", ...)
		# usnLoc <- usnLoc[!grepl("Entry|Pedigree|Rank|Overall", names(usnLoc))]

		yldLstat <- list()
		l <- 1
		for(i in names(yldLoc)[!names(yldLoc) %in% c("Line", "Entry", "OverLocations")]){
			di <- addTraitPercRank(yldLoc[c("Line", i)], i, ties.method = "first")[-1]
			# names(di) <- paste0(substr(names(di), 1, 1), gsub(".*_", "", names(di)))
			di[,3] <- as.integer(round(di[,3]))
			yldLstat[[l]] <- di
			l <- l + 1
		}
		yldSum <- data.frame(yldLoc["Line"], do.call(cbind, yldLstat))

		checks <- c(checks, "mean", "LSD", "CV")

		yldCheckSum <- yldSum[yldSum$Line %in% checks, ]
		dim(yldCheckSum)

		breaks <- seq(1, ncol(yldCheckSum), by = 3*nWide)
		if(breaks[length(breaks)] < ncol(yldCheckSum)){
			breaks <- c(breaks, ncol(yldCheckSum))
		}
		if(printMeans){
			for(i in 2:length(breaks)) {
				l <- list(...)
				if("con" %in% names(l)) {
					xtable2(yldCheckSum[c(1, {breaks[i-1]+1}:{breaks[i]})], include.rownames = FALSE, con = paste0(gsub("\\.tex", "", l[["con"]]), "ByLoc", i-1, ".tex"))
				} else {
					xtable2(yldCheckSum[c(1, {breaks[i-1]+1}:{breaks[i]})], include.rownames = FALSE)
				}
			}
		}
		rL <- list(OverLoc = rL, inLoc = yldSum)
	}
	
	return(rL)
}
