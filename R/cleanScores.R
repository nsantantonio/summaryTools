#' cleanScores function
#'
#' function to (do something)
#'
#' @param fb [value]
#' @param scoreTraits [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
cleanScores <- function(fb, scoreTraits = NULL, zeroThreshold = 0.5){
	isScore <- function(x){
		if(is.numeric(x)){
			s <- x >= 0 & x <= 9
			# manyMiss <- sum(is.na(x)) / length(x) > zeroThreshold
			some <- any(!is.na(x))
			s[is.na(s)] <- TRUE
			# iss <- all(s) & some & manyMiss
			iss <- all(s) & some
			return(iss)
		} else {
			return(FALSE)
		}
	}
	if(is.null(scoreTraits)) {
		scoreTraits <- names(fb)[sapply(fb, isScore)]
		scoreTraits <- scoreTraits[!grepl("Bloc|Rep", scoreTraits, ignore.case = TRUE)]
		scoreTraits <- gsub("\n.*", "", scoreTraits)
		# if(length(scoreTraits) > 0) message("No score traits provided. The following traits were determined to be scores with values between 0 and 9.")
		# print(scoreTraits)
	}
	if(length(scoreTraits)){
		scoreCols <- grep(paste(scoreTraits, collapse = "|"), names(fb))
		percMiss <- lapply(fb[scoreCols], function(x) sum(is.na(x)) / length(x))
		# needZeros <- names(fb)[scoreCols][percMiss > 0 & percMiss < 1]
		needZeros <- names(fb)[scoreCols][percMiss >= zeroThreshold & percMiss < 1]
		for(i in needZeros){
			fb[is.na(fb[[i]]), i] <- 0
		}
		if(length(needZeros) > 0) {
			message("The following traits were assumed to need scores of 0 imputed based on missingness:")
			print(needZeros)
		}
	}	
	return(fb)
}
