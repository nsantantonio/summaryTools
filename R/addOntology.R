#' cleanTraitNames function
#'
#' function to (do something)
#'
#' @param testData [value]
#' @param ontology [value]. Default is TRUE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
addOntology <- function(testData, ontology){
	if(class(testData) == "data.frame"){
		if(!"trialName" %in% names(testData)) stop("'trialName' not found in 'testData'")
		testData <- list(testData)
		testNames <- unique(gsub("_.*", "", testData[[1]]$trialName))
		if(length(testNames) > 1) testData <- split(testData[[1]], gsub("_.*", "", testData[[1]]$trialName))
		names(testData) <- testNames
	}
	for(i in names(testData)){
		for(j in 1:nrow(ontology)){
			matches <- grepl(ontology$pattern[j], names(testData[[i]]))
			if(!ontology$ontology[j] %in% names(testData[[i]]) & sum(matches) == 1){				
				names(testData[[i]])[grep(ontology$pattern[j], names(testData[[i]]))] <- ontology$ontology[j]
			} else if (sum(matches) > 1) {
				stop(paste0("Multiple traits matched pattern '", ontology$pattern[j], "'"))
			}
		}
	}
	return(testData)
}
