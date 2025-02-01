#' numAwnToChar function
#'
#' function to select an awn type from multiple observations. Intended to be used from a call from cleanAwns(). TA/AL conflicts are assumed to be TA, while TA/A  are assumed to be awned. A/AL conflicts will be printed as A,AL
#'
#' @param x vector of awn scores, either numeric, character or mixed
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
numAwnToChar <- function(x){
	if(class(x) != "character") x <- as.character(x)
	num <- c("1", "2", "3", "4")
	chr <- c("AL", "TA", "A", "M")
	for(i in 1:length(num)){
		x[x == num[i]] <- chr[i]
	}
	return(x)
}