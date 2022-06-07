#' formatTime function
#'
#' function to (do something)
#'
#' @param x [value]
#' @param year [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
formatTime <- function(x, year){
	time <- gsub(paste0("/", substr(year, 3, 4), "\\s"), paste0("/", year, " "), x)		
	# noLeadingZero <- !grepl("^0", time)
	# time[noLeadingZero] <- paste0("0", time[noLeadingZero])
	noSec <- !grepl(":[0-9].*:", time)
	
	newtime <- .POSIXct(rep(NA, length(time)))
	ampm <- grepl("AM|PM", time)
	if(length(ampm)){
		time[noSec & ampm] <- gsub("(\\s[A-z])", ":00\\1", time[noSec & ampm])
		newtime[ampm] <- strptime(time[ampm], "%m/%d/%Y %I:%M:%S %p")
	} 
	time[noSec & !ampm] <- paste0(time[noSec & !ampm], ":00")
	newtime[!ampm] <- strptime(time[!ampm], "%m/%d/%Y %H:%M:%S")
	return(newtime)
}
