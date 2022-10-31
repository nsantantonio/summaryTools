#' consensusTW function
#'
#' function to create a consensus test weight and moisture
#'
#' @param djhm data.frame with 'Moisture', 'TestWeight' from th Dickey-John, and 'Moisture_HarvM' and 'TestWeight_HarvM' from the harvest master. 
#' @param defaultDJ logical. Should default values of moisture and test weight be from the dickey john? Harvest master otherwise.
#' @param TWrange numeric. vector of length 2 to determeine valid test weights. default is c(55, 64)
#' @param barley logical. Is barley?
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
consensusTW <- function(djhm, defaultDJ = FALSE, TWrange = c(55, 64), barley = FALSE, ...){
	if(barley & TWrange[1] == 55) {
		TWrange[1] <- 40 
	}
	if(defaultDJ){
		defM <- "Moisture"
		defTW <- "TestWeight" 
		altM <- "Moisture_HarvM"
		altTW <- "TestWeight_HarvM" 
	} else {
		defM <- "Moisture_HarvM"
		defTW <- "TestWeight_HarvM" 
		altM <- "Moisture"
		altTW <- "TestWeight" 
	}

	missM <- is.na(djhm[[defM]])
	missTW <- is.na(djhm[[defTW]])
	

	if(any(missM)){	
		djhm[[defM]][missM] <- djhm[[altM]][missM]
	}
	if(any(missTW)){
		djhm[[defTW]][missTW] <- djhm[[altTW]][missTW]
	}

	outsideRange <- djhm[[defTW]] < TWrange[1] | djhm[[defTW]] > TWrange[2]
	outsideRange[is.na(outsideRange)] <- FALSE
	if(any(outsideRange)){
		insideRangeAlt <- djhm[[altTW]] > TWrange[1] & djhm[[altTW]] < TWrange[2]
		insideRangeAlt[is.na(insideRangeAlt)] <- FALSE
		needReplaced <- outsideRange & insideRangeAlt
		if(any(needReplaced)){
			djhm[[defTW]][needReplaced] <- djhm[[altTW]][needReplaced]
		}
	}

	consTW <- djhm[c("plot_name", defM, defTW)]
	names(consTW) <- gsub("_HarvM", "", names(consTW))
	return(consTW)
}
