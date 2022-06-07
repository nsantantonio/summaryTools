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
#' @examples none
#' @export
oneYearOverLocSummary <- function(dF, traits, locs = NULL, sortby = NULL, allowDupEnt = TRUE, ...){

	if(any(table(dF$Line) > 1)){
		
		traits <- gsub("\\s*\\(.*|\\s*\n", "", traits)
		trtCols <- grep(paste(traits, collapse = "|"), names(dF))
		traitNames <- names(dF)[trtCols]

		names(dF)[names(dF) %in% traitNames] <- trimws(gsub("\n.*|\\(.*", "", names(dF)[names(dF) %in% traitNames]))
		# names(dF)[names(dF) %in% traitNames] <- gsub("\n.*", "", names(dF)[names(dF) %in% traitNames])

		trtnu <- cleanTraitNames(traitNames)

		if(!is.null(locs)){
			dF <- dF[grep(paste(locs, collapse = "|"), dF$Trial), ]
		}

		lineVar <- names(dF)[grep("^line", names(dF), ignore.case = TRUE)] 
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
		dF$Trial <- factor(dF$Trial, levels = unique(dF$Trial))
		dF$Bloc <- factor(dF$Bloc, levels = unique(dF$Bloc))
		BLUP <- list()
		BLUE <- list()
		unrepTrait <- list()
		for(i in traits){
			message(paste0("Running analyses for trial:", unique(gsub("_[^_]+$", "", dF$Trial)), " trait:", i))
			# message(paste0("Running analyses for trait:", i))
			dfi <- whichTrials(dF, i)
			dfi <- dfi[!is.na(dfi[[i]]),] # added after last years analysis to deal with traits measured in one loc, one block. 

			if(nrow(dfi) > 0){
				whichNameUnit <- which(traits == i)
				# whichNameUnit <- grep(i, traitNames)
				traitNameUnit <- trimws(paste(sapply(trtnu, "[[", whichNameUnit), collapse = " "))
				if(!is.null(sortby)){
					if(length(grep(sortby, i, ignore.case = TRUE))) {
						sortByTrt <- traitNameUnit
					}
				} else {
					sortByTrt <- NULL
				}
				if(any(table(dfi$Line) > 1)){

					if(length(unique(dfi$Trial)) == 1 & length(unique(dfi$Bloc)) == 1){
						fixFormi <- "~ Line"
						mixFormi <- "~ (1|Line)"
					} else if(length(unique(dfi$Trial)) == 1){
						fixFormi <- "~ Bloc + Line"
						mixFormi <- "~ (1|Bloc) + (1|Line)"
					} else if(length(unique(dfi$Bloc)) == 1){
						fixFormi <- "~ Trial + Line"
						mixFormi <- "~ Trial + (1|Line)"
					} else {
						fixFormi <- "~ Trial + Trial:Bloc + Line"
						mixFormi <- "~ Trial + (1|Trial:Bloc) + (1|Line)"
					}

					form <- formula(paste0(i, fixFormi))
					fixedFit <- lm(form, data = dfi)

					BLUE[[traitNameUnit]] <- fitlsmeans(form, "Line", data = dfi)

					rform <- formula(paste0(i, mixFormi))
					BLUP[[traitNameUnit]] <- fitBlupH2(rform, "Line", addMu = TRUE, data = dfi)
				} else {
					unr <- dfi[[i]]
					names(unr) <- dfi[["Line"]]
					if(!all(names(unr) == names(BLUE[[1]][[1]]))) unr <- unr[names(BLUE[[1]][[1]])]
					BLUE[[traitNameUnit]] <- list(BLUE = unr, mean = mean(unr), CV = NA, LSD = NA)
					BLUP[[traitNameUnit]] <- list(BLUP = unr, mean = mean(unr), h2 = NA, sigma = NA)
					message(paste0(i, " is unreplicated (only scored in one environment and/or rep). Returning raw phenotypes"))
				}
			}
		}

		lsmeansTable <- makeBLUtab(BLUE, sortby = sortByTrt, ...)
		blupTable <- makeBLUtab(BLUP, sortby = sortByTrt, ...)
		names(lsmeansTable)[names(lsmeansTable) == "effect"] <- "Line"
		names(blupTable)[names(blupTable) == "effect"] <- "Line"
		return(list(BLUE = lsmeansTable, BLUP = blupTable))
	} else {
		message(paste0(unique(dF$Trial), " is an unreplicated trial. Returning nothing"))
		return(NULL)
	}
}
