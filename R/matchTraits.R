#' matchTraits function
#'
#' function to match trait names with or without units, numbers of environments, etc. Spaces are ignored, but error can result if one trait has a name that is nested within another, e.g 'Plant' and 'PlantHeight'. 
#'
#' @param traitNames character vector of trait names, typically as column names in a summary data.frame, these would often have a pipe, "|", and a unit, and possibly the number of environments in parentheses after the trait name which would prevent direct matching with more simple trait names, Example, "GrainYield|lb/ac (12)"
#' @param traits character vector of traits, typically these would not have units or the number of environments, e.g. "Grain Yield"
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
matchTraits <- function(traitNames, traits, warnMultiple = TRUE){
	traits <- gsub("\\s|\\|.*", "", traits)
	isMatch <- NULL
	for(i in traitNames){
		mch <- grepl(paste0("^", gsub("\\s|\\|.*", "", i)), traits)
		if(sum(mch) > 1 & warnMultiple) warning("More than one trait match! Unexpected results may occur")
		isMatch[i] <- any(mch) 
	}
	return(isMatch)
}