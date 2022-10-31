#' cleanTraitNames function
#'
#' function to (do something)
#'
#' @param traitNames [value]
#' @param splitPattern [value]. Pattern that separates triat name from units or ontology
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
cleanTraitNames <- function(traitNames, splitPattern = "\n|\\|"){
	#### this requires that trait names have the form trait\nunit. It will fail wihout the new line!
	# if(addUnits){
	# 	traitNames[grep("Yield", traitNames, ignore.case = TRUE)] <- paste0(traitNames[grep("Yield", traitNames, ignore.case = TRUE)], "\n(bu/ac)")
	# 	traitNames[grep("TestWeight", traitNames, ignore.case = TRUE)] <- paste0(traitNames[grep("TestWeight", traitNames, ignore.case = TRUE)], "\n(lb/bu)")
	# 	traitNames[grep("Moisture", traitNames, ignore.case = TRUE)] <- paste0(traitNames[grep("Moisture", traitNames, ignore.case = TRUE)], "\n(%)")
	# }

# 	lb <- grepl("\n", traitNames)
# 	nb <- grepl("\\(", traitNames) & !lb
# 	traitUnits <- rep("", length(traitNames))
# 	if(any(grepl("\n", traitNames))) {
# 		splitNames <- strsplit(traitNames, "\n")	
# 		traitUnits[lb] <- sapply(splitNames[lb], "[[", 2)
# 	}
# 	if(any(grepl("\\(", traitNames))) {
# 		splitNames <- strsplit(traitNames, "\\(")
# 		traitUnits[nb] <- paste0("(", sapply(splitNames[nb], "[[", 2))
# 	}
# 	traits <- trimws(gsub("\n|\\(.*", "", traitNames))
# 	list(traits = traits, units = traitUnits)

	# traitNames <- c("Grain Moisture|%", "Test Weight\nlb/bu", "Plot Weight (g)", "Grain Yield|bu/ac", "Heading Date|Julian", "Height|in", "Lodging|0-9", "Powdery Mildew|0-9")
	# traitNames <- traits; splitPattern = "\n|\\|"
	lb <- grepl(splitPattern, traitNames)
	nb <- grepl("\\(", traitNames) & !lb
	traitUnits <- rep("", length(traitNames))
	simpleTraitNames <- rep("", length(traitNames))
	
	# if(all(!lb) & all(!nb)){
	# 	simpleTraitNames <- traitNames
	# }
	if(any(!lb & !nb)){
		simpleTraitNames[!lb & !nb] <- traitNames[!lb & !nb]
	}
	if(any(lb)) {
		splitNames <- strsplit(traitNames, splitPattern)	
		traitUnits[lb] <- sapply(splitNames[lb], "[[", 2)
		simpleTraitNames[lb] <- sapply(splitNames[lb], "[[", 1)
	}
	if(any(grepl("\\(", traitNames))) {
		splitNames <- strsplit(traitNames, "\\(")
		traitUnits[nb] <- paste0("(", sapply(splitNames[nb], "[[", 2))
		simpleTraitNames[nb] <- sapply(splitNames[nb], "[[", 1)
	}
	simpleTraitNames <- gsub("\\s", "", simpleTraitNames)
	if(any(simpleTraitNames == "")) stop("lost triat name using cleanTraitNames(). Check that trait name is ok.")
	list(traits = simpleTraitNames, units = traitUnits)
}
