#' oneYearOneLocSummary function
#'
#' function to (do something)
#'
#' @param dF [value]
#' @param traits [value]
#' @param sortby [value]. Default is NULL
#' @param allowDupEnt [value]. Default is TRUE
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
oneYearOneLocSummary <- function(dF, traits, sortHiLo = NULL, sortLoHi = NULL, allowDupEnt = TRUE, unitSep = "|", verbose = FALSE, fixed = NULL, random = NULL, printFit = FALSE, ...){
	# dF = testData[[k]]; traits = qtraits; addInfo = dfInfo(addEntry, by = "Line"); sortHiLo = sortby; sortLoHi = NULL; unitSep = "|"; allowDupEnt = TRUE
	# dF = mdxn23; traits = c("Grain Yield|bu/ac", "Test Weight|lb/bu")
	if(!all(c("Trial", "Line", "Entry", "Block", "plot_name") %in% names(dF))) stop("input data.frame must have columns 'Trial', 'Line', 'Entry', 'Block' and 'plot_name'") # need to update this! got distrcted and didnt finish
	traits <- gsub("\\s*\\(.*|\\s*\n|\\|.*", "", traits)
	# trtCols <- grep(paste(traits, collapse = "|"), names(dF))
	trtCols <- sapply(traits, function(x) grep(x, names(dF)))
	trtCols <- unlist(trtCols[sapply(trtCols, length) > 0])
	if(is.null(trtCols)) stop("traits provided are not in the data.frame!")

	numtrait <- sapply(dF[trtCols], is.numeric)
	charTrtCols <- trtCols[!numtrait]
	trtCols <- trtCols[numtrait]
	
	traits <- traits[traits %in% names(trtCols)]
	traitNames <- names(dF)[trtCols]


	# names(dF)[names(dF) %in% traitNames] <- gsub("\n.*", "", names(dF)[names(dF) %in% traitNames])
	names(dF)[names(dF) %in% traitNames] <- trimws(gsub("\n.*|\\(.*|\\|.*", "", names(dF)[names(dF) %in% traitNames]))

	trtnu <- cleanTraitNames(traitNames)

	printTrait <- NULL

	trials <- unique(dF$Trial)
	
	estL <- list()
	for(j in trials){
		# j <- trials[2]
		dfj <- dF[dF$Trial == j,]

		lineVar <- names(dfj)[grep("^line", names(dfj), ignore.case = TRUE)] 
		entVar <- names(dfj)[grep("^ent", names(dfj), ignore.case = TRUE)]
		if(lineVar != "Line") names(df)[names(df) == lineVar] <- "Line"
		if(entVar != "Entry") names(df)[names(df) == entVar] <- "Entry"
		lineEnt <- unique(dfj[c("Line", "Entry")])
		lineEnt <- lineEnt[order(lineEnt[["Entry"]]),]

		naLine <- is.na(dF$Line)

		if(any(!is.na(dfj[charTrtCols]))){
			lineEntNotes <- compileNotes(dfj, charTrtCols)
			addNotes <- TRUE
		} else {
			addNotes <- FALSE
		}

		if(any(table(dfj$Line) > 1) & any(!is.na(dfj[trtCols]))){

			if(any(naLine)){
				nadF <- dfj[naLine, "plot_name"]
				dfj <- dfj[!naLine, ]
				message("The following plots did not have a value for 'Line', and were removed from the data set:")
				print(nadF)
			}

			if(any(duplicated(lineEnt[["Line"]])) & !allowDupEnt){
				message(paste0("Duplicate entries found in ", j, "! Each duplicate entry will be treated as a separate line. Use allowDupEnt = TRUE to ignore\n"))
				dupEnt <- lineEnt[["Line"]][duplicated(lineEnt[["Line"]])]
				isDup <- lineEnt[["Line"]] %in% dupEnt
				lineEnt[["Line"]][isDup] <- paste0(lineEnt[["Line"]][isDup], "_Entry", lineEnt[["Entry"]][isDup])
			}

			dfj$Line <- factor(dfj$Line, levels = unique(lineEnt$Line))
			dfj$Trial <- factor(dfj$Trial, levels = unique(dfj$Trial))
			dfj$Bloc <- factor(dfj$Bloc, levels = unique(dfj$Bloc))

			BLUP <- list()
			BLUE <- list()

			sortHiLoTrt <- NULL
			sortLoHiTrt <- NULL
			for(i in traits){
				# i = traits[[5]]
				dfij <- whichTrials(dfj, i)
				dfij <- dfij[!is.na(dfij[[i]]),] # added after last years analysis to deal with traits measured in one loc, one block. 
				if(nrow(dfij) == 0){
					message(paste0("No records for trial: ", j, " trait: ", i, ". Skipping to next trait..."))
					next
				} else {
					message(paste0("Running analyses for trial: ", j, " trait: ", i))
				}

				whichNameUnit <- which(traits == i)
				# whichNameUnit <- grep(i, traitNames)
				traitNameUnit <- trimws(paste(sapply(trtnu, "[[", whichNameUnit), collapse = unitSep))
				if(!is.null(sortHiLo)){
					if(length(grep(sortHiLo, i, ignore.case = TRUE))) {
						sortHiLoTrt <- traitNameUnit
					}
				} else if(!is.null(sortLoHi)){
					if(length(grep(sortLoHi, i, ignore.case = TRUE))) {
						sortLoHiTrt <- traitNameUnit
					}
				}
				if(sum(table(dfij$Line) > 1) > 1){

					if(length(unique(dfij$Trial)) == 1 & length(unique(dfij$Bloc)) == 1){
						fixFormi <- paste0("~ Line", fixed)
						mixFormi <- paste0("~ (1|Line)", random)
					} else if(length(unique(dfij$Trial)) == 1){
						fixFormi <- paste0("~ Bloc + Line", fixed)
						# mixFormi <- "~ (1|Bloc) + (1|Line)"
						mixFormi <- paste0("~ Bloc + (1|Line)", random) # Bloc is fixed here since there usually isnt more than 2 or 3 blocks per, fixed)
					} else {
						fixFormi <- paste0("~ Trial + Trial:Bloc + Line", fixed)
						mixFormi <- paste0("~ Trial + (1|Trial:Bloc) + (1|Line)", random)
					}

					form <- formula(paste0("`", i, "`", fixFormi))
					if(printFit){
						fixedFit <- lm(form, data = dfij)
						print(summary(fixedFit))
					}

					BLUE[[traitNameUnit]] <- fitlsmeans(form, "Line", data = dfij)

					rform <- formula(paste0("`", i, "`", mixFormi))
					BLUP[[traitNameUnit]] <- fitBlupH2(rform, "Line", addMu = TRUE, data = dfij)

				} else {
					unr <- dfij[[i]]
					names(unr) <- dfij[["Line"]]
					LineLev <- levels(dfj[["Line"]])
					unr[LineLev[!LineLev %in% names(unr)]] <- NA
					unr <- unr[LineLev]	
					BLUE[[traitNameUnit]] <- list(BLUE = unr, mean = mean(unr), CV = NA, LSD = NA)
					BLUP[[traitNameUnit]] <- list(BLUP = unr, mean = mean(unr), h2 = NA, sigma = NA)
					message(paste0(i, " is unreplicated (only scored in one environment and/or rep). Returning raw phenotypes"))
				}
				
			}

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
				estL[[j]] <- list(BLUE = lsmeansTable, BLUP = blupTable)
			} else {
				message(paste0(unique(dfj$Trial), " has no numeric phenotypes. Returning nothing"))
			}
		} else {
			message(paste0(unique(dfj$Trial), " is an unreplicated trial. Returning nothing"))
		}
	}
	return(estL)
}