#' oneYearOverLocSummary function
#'
#' function to (do something)
#'
#' @param dF [value]
#' @param traits [value]
#' @param locs [value]. Default is NULL
#' @param sortby [value]. Default is NULL
#' @param allowDupEnt [value]. Default is TRUE
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
oneYearOverLocSummary <- function(dF, traits, locs = NULL, sortHiLo = NULL, sortLoHi = NULL, allowDupEnt = TRUE, unitSep = "|", fixed = NULL, random = NULL, printFit = FALSE, addNoTrial = TRUE, ...){
# dF = testData[[k]]; traits = qtraits; addInfo = dfInfo(addEntry, by = "Line"); sortby = by; allowDupEnt = TRUE; locs = NULL; unitSep = "|"
	if(!all(c("Trial", "Line", "Entry", "Block", "plot_name") %in% names(dF))) stop("input data.frame must have columns 'Trial', 'Line', 'Entry', 'Block' and 'plot_name'") # need to update this! got distrcted and didnt finish
	if(any(table(dF$Line) > 1)){
		
		traits <- gsub("\\s*\\(.*|\\s*\n|\\|.*", "", traits)
		trtCols <- sapply(traits, function(x) grep(x, names(dF)))
		trtCols <- unlist(trtCols[sapply(trtCols, length) > 0])
		if(is.null(trtCols)) stop("traits provided are not in the data.frame!")
			
		numtrait <- sapply(dF[trtCols], is.numeric)
		charTrtCols <- trtCols[!numtrait]
		trtCols <- trtCols[numtrait]
	
		traits <- traits[traits %in% names(trtCols)]
		traitNames <- names(dF)[trtCols]

		names(dF)[names(dF) %in% traitNames] <- trimws(gsub("\n.*|\\(.*|\\|.*", "", names(dF)[names(dF) %in% traitNames]))
		# names(dF)[names(dF) %in% traitNames] <- gsub("\n.*", "", names(dF)[names(dF) %in% traitNames])

		naLine <- is.na(dF$Line)
		if(any(naLine)){
			nadF <- dF[naLine, "plot_name"]
			dF <- dF[!naLine, ]
			message("The following plots did not have a value for 'Line', and were removed from the data set:")
			print(nadF)
		}

		trtnu <- cleanTraitNames(traitNames)

		if(!is.null(locs)){
			dF <- dF[grep(paste(locs, collapse = "|"), dF$Trial), ]
		}

		lineVar <- names(dF)[grep("^line", names(dF), ignore.case = TRUE)] 
		entVar <- names(dF)[grep("^ent", names(dF), ignore.case = TRUE)]
		if(lineVar != "Line") names(df)[names(df) == lineVar] <- "Line"
		if(entVar != "Entry") names(df)[names(df) == entVar] <- "Entry"
		lineEnt <- unique(dF[c("Line", "Entry")])
		lineEnt <- lineEnt[order(lineEnt[["Entry"]]),]
		
		if(any(!is.na(dF[charTrtCols]))){
			lineEntNotes <- compileNotes(dF, charTrtCols)
			addNotes <- TRUE
		} else {
			addNotes <- FALSE
		}

		if(any(duplicated(lineEnt[["Line"]])) & !allowDupEnt){
			message(paste0("Duplicate entries found in ", unique(dF$Trial), "! Each duplicate entry will be treated as a separate line. Use allowDupEnt = TRUE to ignore\n"))
			dupEnt <- lineEnt[["Line"]][duplicated(lineEnt[["Line"]])]
			isDup <- lineEnt[["Line"]] %in% dupEnt
			lineEnt[["Line"]][isDup] <- paste0(lineEnt[["Line"]][isDup], "_Entry", lineEnt[["Entry"]][isDup])
		}
		dF$Line <- factor(dF$Line, levels = unique(lineEnt$Line))
		dF$Trial <- factor(dF$Trial, levels = unique(dF$Trial))
		dF$Bloc <- factor(dF$Bloc, levels = unique(dF$Bloc))
		BLUP <- list()
		BLUE <- list()
		unrepTrait <- list()
		sortHiLoTrt <- NULL
		sortLoHiTrt <- NULL
		for(i in traits){
			message(paste0("Running analyses for trial: ", unique(gsub("_[^_]+$", "", dF$Trial)), " trait: ", i))
			# message(paste0("Running analyses for trait:", i))
			dfi <- whichTrials(dF, i)
			dfi <- dfi[!is.na(dfi[[i]]),] # added after last years analysis to deal with traits measured in one loc, one block. 
			trialNames <- unique(dfi$Trial)
			nTr <- length(trialNames)

			if(nrow(dfi) > 1){
				whichNameUnit <- which(traits == i)
				# whichNameUnit <- grep(i, traitNames)
				traitNameUnit <- trimws(paste(sapply(trtnu, "[[", whichNameUnit), collapse = unitSep))
				if(addNoTrial) traitNameUnit <- paste0(traitNameUnit, " (", nTr, ")")
				if(!is.null(sortHiLo)){
					if(length(grep(sortHiLo, i, ignore.case = TRUE))) {
						sortHiLoTrt <- traitNameUnit
					}
				} else if(!is.null(sortLoHi)){
					if(length(grep(sortLoHi, i, ignore.case = TRUE))) {
						sortLoHiTrt <- traitNameUnit
					}
				}
				if(any(table(dfi$Line) > 1)){

					if(length(unique(dfi$Trial)) == 1 & length(unique(dfi$Bloc)) == 1){
						fixFormi <- paste0("~ Line", fixed)
						mixFormi <- paste0("~ (1|Line)", random)
					} else if(length(unique(dfi$Trial)) == 1){
						fixFormi <- paste0("~ Bloc + Line", fixed)
						mixFormi <- paste0("~ (1|Bloc) + (1|Line)", random)
					} else if(length(unique(dfi$Bloc)) == 1){
						fixFormi <- paste0("~ Trial + Line", fixed)
						mixFormi <- paste0("~ Trial + (1|Line)", random)
					} else {
						fixFormi <- paste0("~ Trial + Trial:Bloc + Line", fixed)
						mixFormi <- paste0("~ Trial + (1|Trial:Bloc) + (1|Line)", random)
					}

					form <- formula(paste0("`", i, "`", fixFormi))
					if(printFit) {
						fixedFit <- lm(form, data = dfi)
						print(summary(fixedFit))
					}
					BLUE[[traitNameUnit]] <- fitlsmeans(form, "Line", data = dfi)

					rform <- formula(paste0("`", i, "`", mixFormi))
					BLUP[[traitNameUnit]] <- fitBlupH2(rform, "Line", addMu = TRUE, data = dfi)
				} else {
					unr <- dfi[[i]]
					names(unr) <- dfi[["Line"]]
					if(length(BLUE) == 0) {
						unr <- unr[levels(dfi[["Line"]])]
					} else {
						if(!all(names(unr) == names(BLUE[[1]][[1]]))) unr <- unr[names(BLUE[[1]][[1]])]
					}
					BLUE[[traitNameUnit]] <- list(BLUE = unr, mean = mean(unr), CV = NA, LSD = NA)
					BLUP[[traitNameUnit]] <- list(BLUP = unr, mean = mean(unr), h2 = NA, sigma = NA)
					message(paste0(i, " is unreplicated (only scored in one environment and/or rep). Returning raw phenotypes"))
				}
			} else if (nrow(dfi) == 1) {
				warning("Data.frame only has one row:")
				print(dfi)
				message("You may have a single record in a trait that wasnt scored (e.g. Lodging which defaults to 50...")
				# next
			}
		}
		if(length(BLUE) == 0){
			message(paste0("No summary was [produced for ", unique(dF$Trial), ". Check that you dont have a trial with a single phenotype."))
			return(NULL)
		} else {
			lsmeansTable <- makeBLUtab(BLUE, sortHiLo = sortHiLoTrt, sortLoHi = sortLoHiTrt, ...)
			blupTable <- makeBLUtab(BLUP, sortHiLo = sortHiLoTrt, sortLoHi = sortLoHiTrt, ...)
			names(lsmeansTable)[names(lsmeansTable) == "effect"] <- "Line"
			names(blupTable)[names(blupTable) == "effect"] <- "Line"
			if (addNotes){
				lsmeansTable <- mergeNotes(lsmeansTable, lineEntNotes)
				blupTable <- mergeNotes(blupTable, lineEntNotes)
			}
			if (is.null(sortHiLoTrt) & is.null(sortLoHiTrt) & "Entry" %in% names(lsmeansTable)) {
				lsmeansTable <- lsmeansTable[order(lsmeansTable[["Entry"]]),]
				blupTable <- blupTable[order(blupTable[["Entry"]]),]
			# use of mergeNotes should fix this need to resort afterward.
			# } else if (!is.null(sortByTrt) & addNotes){
			# 	if(!is.null(sortHiLo)) neg <- -1 else neg <- 1
			# 	lsmeansTable <- lsmeansTable[order(neg * lsmeansTable[[sortByTrt]]),]
			# 	blupTable <- blupTable[order(neg * lsmeansTable[[sortByTrt]]),]
			}

			return(list(BLUE = lsmeansTable, BLUP = blupTable))
		}
	} else {
		message(paste0(unique(dF$Trial), " is an unreplicated trial. Returning nothing"))
		return(NULL)
	}
}
