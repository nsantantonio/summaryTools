#' make9colDJ function
#'
#' function to (do something)
#'
#' @param dj3col [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
make9colDJ <- function(dj3col){
	columnNames <- c("Sample.ID", "Moisture", "Weight", "Temperature", "Product", "Issue.No.", "Customer.ID", "User.ID", "Date.Time")
	headers <- sapply(c("plot|sample", "moist", "tw|weight"), function(x) {grep(x, names(dj3col), ignore.case = TRUE)})
	dj3col <- dj3col[headers]
	names(dj3col) <- c("Sample.ID", "Moisture", "Weight")

	for(i in columnNames){
		if(!i %in% names(dj3col)){
			dj3col[[i]] <- ""
		}
	}
	dj3col[["Date.Time"]] <- "01/01/2010 12:01 AM"
	return(dj3col[columnNames])
}
