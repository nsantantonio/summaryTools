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
#' @examples none
#' @export
oneYearOneLocSummary <- function(dF, traits, sortby = NULL, allowDupEnt = TRUE, unitSep = "|", ...){
	# dF = testData[[k]]; traits = traits; addInfo = dfInfo(addEntry, by = "Line"); sortby = by; unitSep = "|"; allowDupEnt = TRUE
	traits <- gsub("\\s*\\(.*|\\s*\n|\\|.*", "", traits)
	# trtCols <- grep(paste(traits, collapse = "|"), names(dF))
	trtCols <- sapply(traits, function(x) grep(x, names(dF)))
	trtCols <- unlist(trtCols[sapply(trtCols, length) > 0])
	traits <- traits[traits %in% names(trtCols)]
	traitNames <- names(dF)[trtCols]

	# names(dF)[names(dF) %in% traitNames] <- gsub("\n.*", "", names(dF)[names(dF) %in% traitNames])
	names(dF)[names(dF) %in% traitNames] <- trimws(gsub("\n.*|\\(.*|\\|.*", "", names(dF)[names(dF) %in% traitNames]))

	trtnu <- cleanTraitNames(traitNames)

	printTrait <- NULL

	trials <- unique(dF$Trial)
	
	estL <- list()
	for(j in trials){

		dfj <- dF[dF$Trial == j,]

		if(any(table(dfj$Line) > 1) & any(!is.na(dfj[trtCols]))){

			lineVar <- names(dfj)[grep("^line", names(dfj), ignore.case = TRUE)] 
			entVar <- names(dfj)[grep("^ent", names(dfj), ignore.case = TRUE)]
			lineEnt <- unique(dfj[c(lineVar, entVar)])
			lineEnt <- lineEnt[order(lineEnt[[entVar]]),]

			if(any(duplicated(lineEnt[[lineVar]])) & !allowDupEnt){
				message(paste0("Duplicate entries found in ", j, "! Each duplicate entry will be treated as a separate line. Use allowDupEnt = TRUE to ignore\n"))
				dupEnt <- lineEnt[[lineVar]][duplicated(lineEnt[[lineVar]])]
				isDup <- lineEnt[[lineVar]] %in% dupEnt
				lineEnt[[lineVar]][isDup] <- paste0(lineEnt[[lineVar]][isDup], "_Entry", lineEnt[[entVar]][isDup])
			}

			dfj$Line <- factor(dfj$Line, levels = unique(lineEnt$Line))
			dfj$Trial <- factor(dfj$Trial, levels = unique(dfj$Trial))
			dfj$Bloc <- factor(dfj$Bloc, levels = unique(dfj$Bloc))

			BLUP <- list()
			BLUE <- list()

			sortByTrt <- NULL
			for(i in traits){
				
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
				if(!is.null(sortby)){
					if(length(grep(sortby, i, ignore.case = TRUE))) {
						sortByTrt <- traitNameUnit
					}
				} else {
					sortByTrt <- NULL
				}
				if(any(table(dfij$Line) > 1)){

					if(length(unique(dfij$Trial)) == 1 & length(unique(dfij$Bloc)) == 1){
						fixFormi <- "~ Line"
						mixFormi <- "~ (1|Line)"
					} else if(length(unique(dfij$Trial)) == 1){
						fixFormi <- "~ Bloc + Line"
						mixFormi <- "~ (1|Bloc) + (1|Line)"
					} else {
						fixFormi <- "~ Trial + Trial:Bloc + Line"
						mixFormi <- "~ Trial + (1|Trial:Bloc) + (1|Line)"
					}

					form <- formula(paste0("`", i, "`", fixFormi))
					fixedFit <- lm(form, data = dfij)

					BLUE[[traitNameUnit]] <- fitlsmeans(form, "Line", data = dfij)

					rform <- formula(paste0("`", i, "`", mixFormi))
					BLUP[[traitNameUnit]] <- fitBlupH2(rform, "Line", addMu = TRUE, data = dfij)

				} else {
					unr <- dfij[[i]]
					names(unr) <- dfij[["Line"]]
					if(length(unr) < length(BLUE[[1]][[1]])) unr[names(BLUE[[1]][[1]])[!names(BLUE[[1]][[1]]) %in% names(unr)]] <- NA
					unr <- unr[names(unr) %in% names(BLUE[[1]][[1]])]
					if(!all(names(unr) == names(BLUE[[1]][[1]]))) unr <- unr[names(BLUE[[1]][[1]])]
					BLUE[[traitNameUnit]] <- list(BLUE = unr, mean = mean(unr), CV = NA, LSD = NA)
					BLUP[[traitNameUnit]] <- list(BLUP = unr, mean = mean(unr), h2 = NA, sigma = NA)
					message(paste0(i, " is unreplicated (only scored in one environment and/or rep). Returning raw phenotypes"))
				}
				
			}

			if(length(BLUE)){			
				lsmeansTable <- makeBLUtab(BLUE, sortby = sortByTrt, ...)
				blupTable <- makeBLUtab(BLUP, sortby = sortByTrt, ...)
				names(lsmeansTable)[names(lsmeansTable) == "effect"] <- "Line"
				names(blupTable)[names(blupTable) == "effect"] <- "Line"
				if (is.null(sortByTrt) & entVar %in% names(lsmeansTable)) {
					lsmeansTable <- lsmeansTable[order(lsmeansTable[[entVar]]),]
					blupTable <- blupTable[order(blupTable[[entVar]]),]
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