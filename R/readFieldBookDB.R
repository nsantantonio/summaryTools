#' readFieldBookDB function
#'
#' function to read database files exported by the Fieldbook Application
#'
#' @param path character. Path to file or directory containing multiple database files exported from Fieldbook app. 
#' @param ontology data.frame. data.frame with two variables, 'pattern' as the regrular expression to match with each each trait, paired to an 'ontology', which should have a pipe and an ontology number (eg. 'my trait|CO123:456789')
#' @param printTraitTable logical, should the table of traits be printed? default is FALSE,
#' @param exportNA logical. should records with missing information be printed? default is FALSE 
#' @param printDup logical. should duplicate records be printed? default is FALSE 
#' @param exportFullFB logical. Should the entire fieldbook be exported (i.e. no duplicate or missing data records removed)? default is  FALSE 
#' @return data.frame of processed and sorted field book records
#' @details [fill in details here]
#' @examples # none
#' @export
readFieldBookDB <- function(path, ontology = NULL, printTraitTable = FALSE, exportNA = FALSE, printDup = FALSE, exportFullFB = FALSE, printDupPairs = TRUE, exportDupPairs = FALSE){
	# c("plot_name", "trait", "value", "timestamp", "person", "location", "number")
	# path = toCleanDir; printDup = TRUE; printTraitTable = FALSE; exportNA = FALSE; printDup = TRUE; exportFullFB = FALSE; printDupPairs = TRUE; exportDupPairs = FALSE
	if(file.exists(path) & !dir.exists(path)){
		fb <- read.csv(file)	
	} else if(file.exists(path) & dir.exists(path)) {
		files <- list.files(path)
		files <- files[!grepl("^trait", files)]
		fbdb <- files[grep("database\\.csv$", files)]
		fbtab <- gsub("database\\.csv$", "table.csv", fbdb)
	
		dt <- strsplit(gsub("_.*", "", files), "-")
		dt <- dt[sapply(dt, length) == 6]
		exportDate <- sapply(lapply(dt, "[", 1:3), paste, collapse = "-")
		exportTime <- sapply(lapply(dt, "[", 4:6), paste, collapse = ":")
		exportDateTime <- as.POSIXlt(paste(exportDate, exportTime))
		
		fbL <- list()
		for(i in fbdb){
			fbi <- read.csv(paste0(path, i))
			fbi$file <- i
			names(fbi)[names(fbi) == "plotName"] <- "plot_name"
			fbL[[i]] <- fbi
		}
		# unique(lapply(fbL, names))
		fb <- do.call(rbind, fbL)
		row.names(fb) <- NULL

		if(exportFullFB) return(fb)

		fb[fb$value %in% "","value"] <- NA
		if(exportNA){
			fbNA <- fb[is.na(fb$value),]
		}
		fb <- fb[!is.na(fb$value),]

		if(!is.null(ontology)){
			if(!all(names(ontology) == c("pattern", "ontology"))) stop("'ontology' data.frame must include the columns 'pattern' and 'ontology'")
			for (i in 1:nrow(ontology)){
				fb$trait[grep(ontology[["pattern"]][i], fb$trait)] <- ontology[["ontology"]][i]
			}
		}
		if(printTraitTable) print(table(fb$trait))

		dupRec <- duplicated(fb[c("plot_name", "trait", "value", "number")])
		# sum(dupRec)
		# head(fb[dupRec,])
		fb <- fb[!dupRec,]
		fb$recordNo <- 1:nrow(fb)
		isPic <- grepl("\\.jpg$", fb$value)
		dupRecConf <- duplicated(fb[c("plot_name", "trait")]) & !isPic
		fbdup <- fb[dupRecConf,]
		if(printDupPairs){
			fbdupPairs <- list()
			for(i in 1:nrow(fbdup)){
				fbdupPairs[[i]] <- fb[fb$plot_name %in% fbdup$plot_name[i] & fb$trait %in% fbdup$trait[i],]
			}
		}
		# fb[fb$plot_name == "HRWPrelim_2022_WARVA_136",]		
		# table(fb[dupRecConf,"trait"])
		# head(fb[dupRecConf,], 20)

		if(nrow(fbdup) < 11){
			message("The following records are duplicates with conflicting values, assigning value of last timestamp...")
			print(fbdup)
		} else {
			message(paste0(nrow(fbdup), " records are duplicates with conflicting values, assigning value of last timestamp. Use 'printDup' to print duplicated records"))
			if(printDup) {
				if(printDupPairs) {
					print(fbdupPairs)
				} else {
					print(fbdup) 
				}
			}
		}

		getLastRecord <- function(dupfbrec, returnRow = FALSE){
			times <- as.POSIXlt(dupfbrec$timestamp) # need to check value type then concatenate if notes, and pick later time point if not notes. 
			keep <- which(times == max(times))
			if(returnRow) return(dupfbrec[keep,]) else return(keep)
		}
		dropRecords <- NULL
		for(i in 1:nrow(fbdup)){
			trait <- fbdup$trait[i]
			dupRows <- which(fb$plot_name == fbdup$plot_name[i] & fb$trait == trait)
			if(trait == "notes"){
				fbd <- fb[dupRows, ]
				allnotes <- paste0(gsub("\\s.*", "", fbd$timestamp), ": ", fb[dupRows, "value"])
				fb[dupRows[1], "value"] <- paste(allnotes, collapse = "; ")
				dropRecords <- c(dropRecords, dupRows[-1])
			} else{
				dropReci <- dupRows[!dupRows %in% dupRows[getLastRecord(fb[dupRows, ])]]
				dropRecords <- c(dropRecords, dropReci)
			}
			# fb <- fb[-dupRows[!dupRows %in% dupRows[getLastRecord(fb[dupRows, ])]],]
		}
		fb[dropRecords,]
		fb <- fb[-dropRecords, ]

		if(any(duplicated(fb[c("plot_name", "trait")]) & !grepl("\\.jpg$", fb$value))) warning("there are still duplicates! This shouldnt happen.")
		
	} else {
		stop("path does not exist!")
	}
	trial <- gsub("_[^_]+$", "", fb$plot_name)
	plotNos <- as.integer(gsub(".*_", "", fb$plot_name))
	fb <- fb[order(trial, fb$trait, plotNos), ]
	if(exportNA){
		fb <- list(fb = fb, missing = exportNA)
	}
	if(exportDupPairs){
		fb <- list(fb = fb, duplicateRecords = fbdupPairs)
	}
	return(fb)
}
