#' write.fbData function
#'
#' function to convert a data.frame of data o fieldook database file. Usual used to get data from colaboators, etc, anyting that was recored on a spreadshet. 
#'
#' @param db data.frame out put from dataToFb function
#' @param path optional chaacter vector of path to where he file should be written 
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
write.fbData <- function(db, path = NULL){
	# db <- dondb
	tm <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
	trials <- unique(gsub("_[^_]+$", "", db$plot_name))
	for(i in trials){
		fname <- paste0(path, tm, "_", i, "_database.csv")
		write.csv(db, file = fname, quote = TRUE, row.names = FALSE)
	}
	#would it be worth pausing and updating time for each trial, that way they cant be overwritten if run at the same time?
}
