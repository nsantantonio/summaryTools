#' strPad function
#'
#' function to pad strings. 
#'
#' @param x characer vector to be padded
#' @param type either r, l or c for left , right or center. 
#' @param pad should additional pad be added?
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
strPad <- function(x, type = "l", pad = 0){
	# x <- c("A", "B", "\\%C\\%D", "E")
	# x = tab[, j]; type = padType[j]; pad = pad
	len <- nchar(x) 
	# len <- len + sapply(gregexpr("\\\\", x), function(x) sum(x > 0))
	len[is.na(len)] <- 0
	maxLen <- max(len, na.rm = TRUE)
	if(type == "c"){
		lpadding <- sapply(len, function(l) paste0(rep(" ", times = floor((maxLen - l + pad)/2)), collapse = ""))
		rpadding <- sapply(len, function(l) paste0(rep(" ", times = ceiling((maxLen - l + pad)/2)), collapse = ""))	
		x <- paste0(lpadding, x, rpadding)
	} else {
		padding <- sapply(len, function(l) paste0(rep(" ", times = maxLen - l + pad), collapse = ""))
		if(type == "r") {
			x <- paste0(padding, x)
		} else {
			x <- paste0(x, padding)
		}
	}
	return(x)
}