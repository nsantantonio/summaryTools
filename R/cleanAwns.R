#' selectAwn function
#'
#' function to select an awn type from multiple observations. Intended to be used from a call from cleanAwns(). TA/AL conflicts are assumed to be TA, while TA/A  are assumed to be awned. A/AL conflicts will be printed as A,AL
#'
#' @param x vector of multiple awn observations of the form "WARVA: A,TA,A"
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
cleanAwns <- function(x){
	x <- trimws(gsub("[A-Z]{3}VA:", "", x))
	sapply(strsplit(x, "\\,|\\s"), selectAwn)
}