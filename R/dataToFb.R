#' dataToFb function
#'
#' function to convert a data.frame of data o fieldook database file. Usual used to get data from colaboators, etc, anyting that was recored on a spreadshet. 
#'
#' @param dF data.frame with plot_name and triat columns. This would tpicaly be data recorded on a spreadshet for whatever reason.
#' @param traits chaacter vector of trait names coresponding to dF columns
#' @param plot_name character vector of length 1, Default is 'plot_name'
#' @param timestamp Not integrated yet. Default is NULL
#' @param person Not integrated yet. Default is NULL
#' @param location Not integrated yet. Default is NULL
#' @param number Not integrated yet. Default is 1
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
dataToFb <- function(dF, traits, plot_name = "plot_name", timestamp = NULL, person = NULL, location = NULL, number = 1){
	if(plot_name != "plot_name") names(dF)[names(dF) == plot_name] <- "plot_name"
	dFt <- dF[c("plot_name", traits)]
	dFL <- list()
	for(i in traits){
		dFi <- dFt[c("plot_name", i)]
		names(dFi)[2] <- "value"
		dFi[[2]] <- as.character(dFi[[2]]) # converts numeric, POSIX, etc to character so subsequent traits do not inherit previous traits class, eg. first trait is POSIX, second is numeric will results in second trait being POSIX
		dFi[["trait"]] <- i
		dFL[[i]] <- dFi[c("plot_name", "trait", "value")]
	}
	dFdb <- do.call(rbind, dFL)
	if(is.null(timestamp)){}
	dFdb$timestamp = NA
	dFdb$person = NA
	dFdb$location = NA
	dFdb$number = 1
	return(dFdb)
}
