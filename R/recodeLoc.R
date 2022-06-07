#' recodeLoc function
#'
#' function to (do something)
#'
#' @param loc [value]
#' @param locCode [value]. Default is NULL
#' @param locPattern [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
recodeLoc <- function(loc, locCode = NULL, locPattern = NULL){
	if(is.null(locCode)){
		locCode <- c("BLAVA", "WARVA", "PNTVA", "BLSVA", "HOLVA", "ORGVA", "SHVVA", "NKTVA")
	}
	if(is.null(locPattern)){
		locPattern <- c("blacksburg", "warsaw", "painter", "blackstone", "holland", "orange", "valley", "kent")
	}
	for(i in 1:length(locCode)){
		loc[grepl(locPattern[i], loc, ignore.case = TRUE)] <- locCode[i]
	}
    return(loc)
}
