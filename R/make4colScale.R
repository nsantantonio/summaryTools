#' make4colScale function
#'
#' function to (do something)
#'
#' @param scl3col [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
make4colScale <- function(scl3col){
	if(!all(names(scl3col) %in% c("plot_name", "weight", "tare", "Notes"))){

		headers <- sapply(c("plot", "weight", "tare"), function(x) {grep(x, names(scl3col), ignore.case = TRUE)})
		if (!all(headers == 1:3)) {
			stop("Scale file must have first three columns named 'plot', 'weight', and 'tare' in that order (not case sensitive)")
		}
		sclNames  <- c("plot_name", "weight", "tare")
		if(length(scl3col) > 3) {
		 	if(length(scl3col) > 4) {
		 		message("Any comments or notes must be in 4th column, all others will be ignored")
		 	}
		} else if(ncol(scl3col) == 3){
			scl3col$Notes <- ""
		}

		sclNames <- c(sclNames, "Notes")
		names(scl3col) <- sclNames
	}
	return(scl3col)
}
