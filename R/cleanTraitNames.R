#' cleanTraitNames function
#'
#' function to (do something)
#'
#' @param traitNames [value]
#' @param addUnits [value]. Default is TRUE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
cleanTraitNames <- function(traitNames, addUnits = TRUE){
	#### this requires that trait names have the form trait\nunit. It will fail wihout the new line!
	if(addUnits){
		traitNames[grep("Yield", traitNames, ignore.case = TRUE)] <- paste0(traitNames[grep("Yield", traitNames, ignore.case = TRUE)], "\n(bu/ac)")
		traitNames[grep("TestWeight", traitNames, ignore.case = TRUE)] <- paste0(traitNames[grep("TestWeight", traitNames, ignore.case = TRUE)], "\n(lb/bu)")
		traitNames[grep("Moisture", traitNames, ignore.case = TRUE)] <- paste0(traitNames[grep("Moisture", traitNames, ignore.case = TRUE)], "\n(%)")
	}

	lb <- grepl("\n", traitNames)
	nb <- grepl("\\(", traitNames) & !lb
	traitUnits <- rep("", length(traitNames))
	if(any(grepl("\n", traitNames))) {
		splitNames <- strsplit(traitNames, "\n")	
		traitUnits[lb] <- sapply(splitNames[lb], "[[", 2)
	}
	if(any(grepl("\\(", traitNames))) {
		splitNames <- strsplit(traitNames, "\\(")
		traitUnits[nb] <- paste0("(", sapply(splitNames[nb], "[[", 2))
	}
	traits <- trimws(gsub("\n|\\(.*", "", traitNames))
	list(traits = traits, units = traitUnits)
}
