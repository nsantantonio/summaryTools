#' selectAwn function
#'
#' function to select an awn type from multiple observations. Intended to be used from a call from cleanAwns(). TA/AL conflicts are assumed to be TA, while TA/A  are assumed to be awned. A/AL conflicts will be printed as A,AL
#'
#' @param x list of multiple awn observations per element e.g.(list(c("A", "TA"), c("A", "A", "A"))) 
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
selectAwn <- function(x) {
	if(length(x) > 1){
		tabx <- table(x)
		x <- names(tabx)[tabx == max(tabx)]
	}
	if(length(x) > 1) {
		if(all(x %in% c("AL", "TA"))){
			return("TA")
		} else if(all(x %in% c("A", "TA"))){
			return("A")
		} else {
		return(paste0(x, collapse = ","))
		}
	} else if(length(x) == 1) {
		return(unique(x))
	} else {
		return("")
	}
}