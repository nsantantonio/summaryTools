#' whichTrials function
#'
#' function to (do something)
#'
#' @param dF [value]
#' @param trait [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
whichTrials <- function(dF, trait){
	hasRec <- unique(dF$Trial[!is.na(dF[[trait]])])
	dF[dF$Trial %in% hasRec,]
}
