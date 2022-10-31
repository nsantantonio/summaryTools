#' readRegionalExcel function
#'
#' function to read Wade's crazy excel table summaries. 
#'
#' @param path file path
#' @param nlineHeader number of lines in the header
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
readRegionalExcel <- function(path, nlineHeader = 2, ...){
	# nlineHeader <- 2
	# tm <- as.data.frame(read_excel("/home/nsant/Dropbox/releases2022/dataSummaries/VA17W-75/19GAWN-Means Report-Final_Harrison.xlsx", sheet = "summary", skip = nlineHeader, col_names = FALSE)); tmh <- as.data.frame(read_excel("/home/nsant/Dropbox/releases2022/dataSummaries/VA17W-75/19GAWN-Means Report-Final_Harrison.xlsx", sheet = "summary", n_max = nlineHeader))
	require(readxl)
	tm <- as.data.frame(read_excel(path, skip = nlineHeader, col_names = FALSE, ...))
	tmh <- as.data.frame(read_excel(path, n_max = nlineHeader, ...))

	cols <- names(tmh)
	cols <- gsub("\\.{3}.*|__.*", "", cols)
	cols <- trimws(gsub("\n", " ", cols))
	cols <- trimws(gsub(" |\\/|\\+", "", cols))
	cols <- trimws(gsub("\\(", " \\(", cols))
	loc <- unlist(c(tmh[1,]))
	names(loc) <- cols
	loc[is.na(loc)] <- ""

	names(tm) <- trimws(paste(cols, loc))

	lineCol <- grep("Line", cols, ignore.case = TRUE)
	entCol <- grep("Ent", cols, ignore.case = TRUE)

	startTrmu <- max(lineCol, entCol) + 1
	traits <- cols[startTrmu:length(cols)]

	N <- grep("mean|average", tm[[lineCol]], ignore.case = TRUE) - 1
	lineNames <- tm[[lineCol]][1:N]

	avg <- {N+1}
	CV <- grep("C\\.*V", tm[[lineCol]], ignore.case = TRUE)
	LSD <- grep("L\\.*S\\.*D", tm[[lineCol]], ignore.case = TRUE)
	statLines <- c(avg, CV, LSD)

	datLines <- 1:N

	stat <- tm[statLines,]
	stat["Line"] <- c("mean", "CV", "LSD")
	dat <- tm[datLines, ]
	smry <- rbind(dat, stat)
	# names(smry) <- trimws(paste(cols, loc))

	dfL <- list()
	scL <- list()
	nL <- NULL
	for(i in unique(traits)){
		whichi <- which(traits == i) + startTrmu - 1
		loci <- loc[whichi]
		dati <- dat[whichi]
		rownames(dati) <- lineNames
		stati <- stat[whichi]
		datMat <- as.matrix(dati)
		if(is.numeric(datMat) & ncol(datMat) > 1){
			# colnames(dati) <- loci
			nL[i] <- ncol(datMat)
			dFi <- data.frame(Trial = rep(loci, each = nrow(dati)), Line = rep(lineNames, times = ncol(dati)))
			dFi[[i]] <- c(datMat)
			dfL[[i]] <- dFi
		} else {
			nL[i] <- ncol(datMat)
			dFi <- data.frame(Line = c(lineNames, stat[["Line"]]), rbind(dati, stati))
			scL[[i]] <- dFi
		}
	}

	trialMeans <- Reduce(function(...) merge(..., by = c("Trial", "Line"), all=TRUE), dfL)
	scores <- Reduce(function(...) merge(..., by = c("Line"), all=TRUE), scL)
	list(summary = smry, long = trialMeans, nTrial = nL, scores = scores)
}
