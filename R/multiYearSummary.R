#' multiYearSummary function
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
multiYearSummary <- function(dF, traits, locs = NULL, sortHiLo = NULL, sortLoHi = NULL, allowDupEnt = TRUE, blockName = "Block", unitSep = "|", addNoTrial = TRUE, ...){
	# dF = sw3yr; traits = trts; locs = NULL; sortHiLo = NULL; sortLoHi = NULL; allowDupEnt = TRUE; blockName = "Block"; unitSep= "|"; addNoTrial = TRUE
	if(any(table(dF$Line) > 1)){

		# traits <- gsub("\\s*\\(.*|\\s*\n|\\|.*", "", traits)
		# trtCols <- sapply(traits, function(x) grep(x, names(dF)))
		# if(any(sapply(trtCols, length) > 1)) {
		# 	stop("Multiple columns match a single trait")
		# }
		# trtCols <- unlist(trtCols[sapply(trtCols, length) > 0])
		# traits <- traits[traits %in% names(trtCols)]
		# traitNames <- names(dF)[trtCols]


		# names(dF)[names(dF) %in% traitNames] <- trimws(gsub("\n.*|\\(.*|\\|.*", "", names(dF)[names(dF) %in% traitNames]))

		if(!all(c("Trial", "Line", "Block") %in% names(dF))) stop("input data.frame must have columns 'Trial', 'Line', 'Block'") # need to update this! got distrcted and didnt finish
		traits <- gsub("\\s*\\(.*|\\s*\n|\\|.*", "", traits)
		# trtCols <- grep(paste(traits, collapse = "|"), names(dF))
		trtCols <- sapply(traits, function(x) grep(paste0("^", x), names(dF)))
		trtCols <- unlist(trtCols[sapply(trtCols, length) > 0])
		
		numtrait <- sapply(dF[trtCols], is.numeric)
		charTrtCols <- trtCols[!numtrait]
		trtCols <- trtCols[numtrait]
		
		traits <- traits[traits %in% names(trtCols)]
		traitNames <- names(dF)[trtCols]


	# names(dF)[names(dF) %in% traitNames] <- gsub("\n.*", "", names(dF)[names(dF) %in% traitNames])
		names(dF)[names(dF) %in% traitNames] <- trimws(gsub("\n.*|\\(.*|\\|.*", "", names(dF)[names(dF) %in% traitNames]))
		trtnu <- cleanTraitNames(traitNames)

		if(!is.null(locs)){
			dF <- dF[grep(paste(locs, collapse = "|"), dF$Trial), ]
		}


		if(any(!is.na(dF[charTrtCols]))){
			lineEntNotes <- compileNotes(dF, charTrtCols)
			addNotes <- TRUE
		} else {
			addNotes <- FALSE
		}

		lineVar <- names(dF)[grep("^line", names(dF), ignore.case = TRUE)] 
		if(any(grepl("^ent", names(dF)))) {
			entVar <- names(dF)[grep("^ent", names(dF), ignore.case = TRUE)]
			lineEnt <- unique(dF[c(lineVar, entVar)])
			lineEnt <- lineEnt[order(lineEnt[[entVar]]),]

			if(any(duplicated(lineEnt[[lineVar]])) & !allowDupEnt){
				message(paste0("Duplicate entries found in ", unique(dF$Trial), "! Each duplicate entry will be treated as a separate line. Use allowDupEnt = TRUE to ignore\n"))
				dupEnt <- lineEnt[[lineVar]][duplicated(lineEnt[[lineVar]])]
				isDup <- lineEnt[[lineVar]] %in% dupEnt
				lineEnt[[lineVar]][isDup] <- paste0(lineEnt[[lineVar]][isDup], "_Entry", lineEnt[[entVar]][isDup])
			}
			dF$Line <- factor(dF$Line, levels = unique(lineEnt$Line))
		} else {
			dF$Line <- factor(dF$Line, levels = unique(dF$Line))
		}
		dF$Trial <- factor(dF$Trial, levels = unique(dF$Trial))
		if(is.null(dF[[blockName]])){
			message("no blocking factor found in data.frame, proceeding without block effect")
			dF[[blockName]] <- 1
		}
		dF[[blockName]] <- factor(dF[[blockName]], levels = unique(dF[[blockName]]))
		
		sortHiLoTrt <- NULL
		sortLoHiTrt <- NULL
		BLUP <- list()
		BLUE <- list()
		unrepTrait <- list()
		for(i in traits){
			message(paste0("Running analyses for trait:", i, " across these trials:"))
			dfi <- whichTrials(dF, i)
			# print(unique(gsub("_[^_]+$", "", dfi$Trial)))
			trialNames <- unique(dfi$Trial)
			nTr <- length(trialNames)
			print(trialNames)
			# message(paste0("Running analyses for trait:", i))
			dfi <- dfi[!is.na(dfi[[i]]),] # added after last years analysis to deal with traits measured in one loc, one block. 

			if(nrow(dfi) > 0){
				whichNameUnit <- which(traits == i)
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

					if(length(unique(dfi$Trial)) == 1 & length(unique(dfi[[blockName]])) == 1){
						fixFormi <- "~ Line"
						mixFormi <- "~ (1|Line)"
					} else if(length(unique(dfi$Trial)) == 1){
						fixFormi <- paste0("~ ", blockName, " + Line")
						mixFormi <- paste0("~ (1|", blockName, ") + (1|Line)")
					} else if(length(unique(dfi[[blockName]])) == 1){
						fixFormi <- "~ Trial + Line"
						mixFormi <- "~ Trial + (1|Line)"
					} else {
						fixFormi <- paste0("~ Trial + Trial:", blockName, " + Line")
						mixFormi <- paste0("~ Trial + (1|Trial:", blockName, ") + (1|Line)")
					}

					form <- formula(paste0("`", i, "`", fixFormi))
					fixedFit <- lm(form, data = dfi)

					BLUE[[traitNameUnit]] <- fitlsmeans(form, "Line", data = dfi)

					rform <- formula(paste0("`", i, "`", mixFormi))
					BLUP[[traitNameUnit]] <- fitBlupH2(rform, "Line", addMu = TRUE, data = dfi)
				} else {
					unr <- dfi[[i]]
					names(unr) <- dfi[["Line"]]
					if(length(unr) < length(BLUE[[1]][[1]])) unr[names(BLUE[[1]][[1]])[!names(BLUE[[1]][[1]]) %in% names(unr)]] <- NA
					unr <- unr[names(unr) %in% names(BLUE[[1]][[1]])]
					if(!all(names(unr) == names(BLUE[[1]][[1]]))) unr <- unr[names(BLUE[[1]][[1]])]
					BLUE[[traitNameUnit]] <- list(BLUE = unr, mean = mean(unr), CV = NA, LSD = NA)
					BLUP[[traitNameUnit]] <- list(BLUP = unr, mean = mean(unr), h2 = NA, sigma = NA)
					message(paste0(i, " is unreplicated (only scored in one environment and/or rep). Returning raw phenotypes"))
				}
			}
		}

		# lsmeansTable <- makeBLUtab(BLUE, sortHiLo = sortHiLoTrt, sortLoHi = sortLoHiTrt, ...)
		# blupTable <- makeBLUtab(BLUP, sortHiLo = sortHiLoTrt, sortLoHi = sortLoHiTrt, ...)
		# names(lsmeansTable)[names(lsmeansTable) == "effect"] <- "Line"
		# names(blupTable)[names(blupTable) == "effect"] <- "Line"
		# return(list(BLUE = lsmeansTable, BLUP = blupTable))
		if (length(BLUE)){			
				# lsmeansTable <- makeBLUtab(BLUE, sortHiLo = sortHiLo, sortLoHi = sortLoHi, addInfo = dfInfo(addEntry, by = "Line"))
				# blupTable <- makeBLUtab(BLUP, sortHiLo = sortHiLo, sortLoHi = sortLoHi, addInfo = dfInfo(addEntry, by = "Line"))
				lsmeansTable <- makeBLUtab(BLUE, sortHiLo = sortHiLoTrt, sortLoHi = sortLoHiTrt, ...)
				blupTable <- makeBLUtab(BLUP, sortHiLo = sortHiLoTrt, sortLoHi = sortLoHiTrt, ...)
				names(lsmeansTable)[names(lsmeansTable) == "effect"] <- "Line"
				names(blupTable)[names(blupTable) == "effect"] <- "Line"
				if(addNotes){
					lsmeansTable <- mergeNotes(lsmeansTable, lineEntNotes)
					blupTable <- mergeNotes(blupTable, lineEntNotes)
				}
				if (is.null(sortHiLoTrt) & is.null(sortLoHiTrt) & "Entry" %in% names(lsmeansTable)) {
					# I am a little surprised the stats at bottom dont mess this up. may need to make more robust. 
					lsmeansTable <- lsmeansTable[order(lsmeansTable[["Entry"]]),]
					blupTable <- blupTable[order(blupTable[["Entry"]]),]
				# use of mergeNotes should fix this need to resort afterward.
				# } else if (!is.null(sortByTrt) & addNotes) {
				# 	if(!is.null(sortHiLo)) neg <- -1 else neg <- 1
				# 	lsmeansTable <- lsmeansTable[order(neg * lsmeansTable[[sortByTrt]]),]
				# 	blupTable <- blupTable[order(neg * lsmeansTable[[sortByTrt]]),]
				}
				est <- list(BLUE = lsmeansTable, BLUP = blupTable)
				return(est)
			} else {
				message(paste0(unique(dF$Trial), " has no numeric phenotypes. Returning nothing"))
			}
	} else {
		message(paste0(unique(dF$Trial), " is an unreplicated trial. Returning nothing"))
		return(NULL)
	}
}
