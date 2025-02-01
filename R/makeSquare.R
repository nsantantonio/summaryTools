#' makeSquare function
#' 
#' function to add dummy plots to a field trial to make it square. Useful for spatial analysis.
#'
#' @param dF data.frame of field trial, 
#' @param dvars character vector of variables to add dummy to
#' @param range character. data.frame variable name to indicate row. Default is 'range'
#' @param pass character. data.frame variable name to indicate column. Default is 'pass'
#' @param dummy character. Stand in value for added plots to make square. Default is 'dummy' 
#' @param by character. dF column name by which to apply square, usually physically  separate locations, blocks, etc. 
#' @return data.frame with added dummy plots to make square. 
#' @details [fill in details here]
#' @examples # none
#' @export

makeSquare <- function(dF, dvars = NULL, range = "range", pass = "pass", dummy = "dummy", imputeFactors = TRUE, by = NULL){
#	dF = yldPreObs; range = "range"; pass = "pass"; by = "blockName"; dvars = c("Line"); dummy = "dummy"
	# dF <- dF[dF[[by]] == "jinkinsCenterV", ];
	if(class(dF$Line) != "factor") {stop("dF column 'Line' must be a factor!")}
	if(any(!c(range, pass) %in% names(dF))) stop("columns 'range' and 'pass' must be in dF!")
	if(is.factor(dF[[range]])) {
		rangeLevs <- levels(dF[[range]])
		dF[[range]] <- as.numeric(dF[[range]])
		rangeFactor <- TRUE

	} else {
		rangeFactor <- FALSE
	}
	if(is.factor(dF[[pass]])) {
		passLevs <- levels(dF[[pass]])
		dF[[pass]] <- as.numeric(dF[[pass]])
		passFactor <- TRUE
	} else {
		passFactor <- FALSE
	}

	if(!is.null(by)){
		dfL <- list()
		for(i in unique(dF[[by]])){
			dfL[[i]] <- makeSquare(dF[dF[[by]] == i, ], range = range, pass = pass, dvars = dvars, dummy = dummy)
			dfL[[i]][is.na(dfL[[i]][[by]]), by] <- i
		}
		dF <- do.call(rbind, dfL)
	} else {
		nr <- max(dF[[range]])
		nc <- max(dF[[pass]])
		P <- matrix(0, nr, nc)
		P[cbind(dF[[range]], dF[[pass]])] <- 1
		pad <- which(!t(P))
		allRangePass <- cbind(c(row(P)), c(col(P)))
		colnames(allRangePass) <- c(range, pass)
		# dim(dF)
		dF  <- merge(dF, allRangePass, by = c(range, pass), all = TRUE)
		if(!is.null(dvars)) {
			for(i in dvars){
				levels(dF[[i]]) <- c(levels(dF[[i]]), dummy)
			}
			dF[pad, dvars] <- dummy
		}
		if(imputeFactors){
			dfclass <- sapply(dF, class)
			needDummyFact <- names(dF)[dfclass %in% "factor" & !names(dF) %in% dvars]
			for(i in needDummyFact){
				for(j in which(is.na(dF[[i]]))){
					if(j == 1) {
						dF[[i]][j] <- dF[[i]][which(!is.na(dF[[i]]))[1]]
					} else {
						dF[[i]][j] <- dF[[i]][j-1]
					}
				}
			}
		}
	}

	if(rangeFactor) dF[[range]] <- factor(dF[[range]], levels = rangeLevs)
	if(passFactor) dF[[pass]] <- factor(dF[[pass]], levels = passLevs)
	return(dF)
}
