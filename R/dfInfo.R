#' dfInfo function
#'
#' function to (do something)
#'
#' @param dF [value]
#' @param by [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
dfInfo <- function(dF, by){
	nam <- dF[[by]]
	l <- list()
	for(i in names(dF)[names(dF) != by]){
		x <- dF[[i]]
		names(x) <- nam
		l[[i]] <- x
	}
	return(l)
}
