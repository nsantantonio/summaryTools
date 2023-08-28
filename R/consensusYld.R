#' consensusYld function
#'
#' function to create a consensus yld measurement from Harvest master and scale data
#'
#' @param sclhm data.frame with 'netWieght' from a scale, and 'netWeight_HarvM' from the harvest master. 
#' @param defaultScale logical. Should default values of netweight be from the scale? Harvest master otherwise.
#' @param yldRange numeric. vector of length 2 to determeine valid netweights. default is c(500, 5000)
#' @param barley logical. Is barley?
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
consensusYld <- function(sclhm, defaultScale = TRUE, yldRange = c(500, 6000), barley = FALSE, ...){
	# sclhm = sclL[[test]][[whichTrial]]; defaultScale = TRUE; yldRange = c(500, 6000); barley = FALSE
	if(barley & yldRange[1] == 52) {
		yldRange[1] <- 40 
	}
	if(defaultScale){
		defW <- "netWeight"
		altW <- "netWeight_HarvM"
	} else {
		defW <- "netWeight_HarvM"
		altW <- "netWeight"
	}

	missW <- is.na(sclhm[[defW]])

	if(any(missW)){	
		sclhm[[defW]][missW] <- sclhm[[altW]][missW]
	}

	outsideRange <- sclhm[[defW]] < yldRange[1] | sclhm[[defW]] > yldRange[2]
	outsideRange[is.na(outsideRange)] <- FALSE
	if(any(outsideRange)){
		insideRangeAlt <- sclhm[[altW]] > yldRange[1] & sclhm[[altW]] < yldRange[2]
		insideRangeAlt[is.na(insideRangeAlt)] <- FALSE
		needReplaced <- outsideRange & insideRangeAlt
		if(any(needReplaced)){
			sclhm[[defW]][needReplaced] <- sclhm[[altW]][needReplaced]
		}
	}

	cons <- sclhm[c("plot_name", defW)]
	names(cons) <- gsub("_HarvM", "", names(cons))
	return(cons)
}
