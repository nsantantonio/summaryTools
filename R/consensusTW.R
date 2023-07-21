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
consensusTW <- function(djhm, defaultDJmoist = TRUE, defaultDJweight = TRUE, TWrange = c(52, 64), barley = FALSE, ...){
	# djhm = djMTW[[2]]
	if(barley & TWrange[1] == 52) {
		TWrange[1] <- 40 
	}
	if(defaultDJmoist){
		defM <- "Moisture"
		altM <- "Moisture_HarvM"
	} else {
		defM <- "Moisture_HarvM"
		altM <- "Moisture"
	}

	if(defaultDJweight){
		defTW <- "TestWeight" 
		altTW <- "TestWeight_HarvM" 
	} else {
		defTW <- "TestWeight_HarvM" 
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
