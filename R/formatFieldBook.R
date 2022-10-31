#' formatFieldBook function
#'
#' function to read database files exported by the Fieldbook Application
#'
#' @param fb data.frame. processed fieldbook from output of readFieldBookDB() function. 
#' @param ontology data.frame. data.frame with two variables, 'pattern' as the regrular expression to match with each each trait, paired to an 'ontology', which should have a pipe and an ontology number (eg. 'my trait|CO123:456789')
#' @param printTraitTable logical, should the table of traits be printed? default is FALSE,
#' @param exportNA logical. should records with missing information be printed? default is FALSE 
#' @param printDup logical. should duplicate records be printed? default is FALSE 
#' @param exportFullFB logical. Should the entire fieldbook be exported (i.e. no duplicate or missing data records removed)? default is  FALSE 
#' @return data.frame of processed and sorted field book records
#' @details [fill in details here]
#' @examples none
#' @export
formatFieldBook <- function(fb, year = NULL){
	canBeNumeric <- function(x) { # taken from https://stackoverflow.com/questions/24129124/how-to-determine-if-a-character-vector-is-a-valid-numeric-or-integer-vector
	    stopifnot(is.atomic(x) | is.list(x)) # check if x is a vector
	    numNAs <- sum(is.na(x))
	    newNAs <- suppressWarnings(sum(is.na(as.numeric(x))))
	    return(newNAs == numNAs)
	}
	# c("plot_name", "trait", "value", "timestamp", "person", "location", "number")
	traits <- unique(fb$trait)
	if(is.null(year)){
		year <- as.integer(unique(sapply(sapply(fb$plot_name, strsplit, "_"), "[", 2)))
	}
	trL <- list()
	for(i in traits){
		# i = traits[1]
		fbi <- fb[fb$trait == i, ]		
		if(grepl("Heading|^HD", i)){
			isDate <- grep("\\-|\\/", fbi$value)
			if(any(isDate)){
				fbi$value[isDate] <- as.integer(round(as.POSIXlt(fbi$value[isDate]) - as.POSIXlt(paste0(year - 1, "-12-31"))))
				fbi$value <- as.integer(fbi$value)
			} 
		}
		names(fbi)[names(fbi) == "value"] <- i
  		if (canBeNumeric(fbi[[i]])) fbi[[i]] <- as.numeric(fbi[[i]])
		dupPlot <- duplicated(fbi[["plot_name"]])
		if(any(dupPlot)){
			fbi[fbi$plot_name %in% fbi$plot_name[dupPlot],]
			dupPlotName <- unique(fbi$plot_name[fbi$plot_name %in% fbi$plot_name[dupPlot]])
			uniqPlot <- list()
			for(j in dupPlotName){
				dupj <- fbi[fbi$plot_name %in% j,]
				dupj[1,i] <- paste(dupj[[i]], collapse = "; ")
				uniqPlot[[j]] <- dupj[1,]
			}
			fbi <- rbind(fbi[!fbi$plot_name %in% dupPlotName, ], do.call(rbind, uniqPlot))
		}
		if(any(duplicated(fbi$plot_name))) {
			print(fbi[fbi$plot_name %in% fbi$plot_name[duplicated(fbi$plot_name)],])
			stop("formatFieldBook: the above plots are duplicated!")
		}
		trL[[i]] <- fbi[c("plot_name", i)]
	}

	fbdf <- Reduce(function(...) merge(..., all = TRUE, by = "plot_name"), trL)
	dim(fbdf)
	return(fbdf)
}

