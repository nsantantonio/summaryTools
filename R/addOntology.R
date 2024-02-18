#' addOntology function
#'
#' function to change trait names to add ontology numbers.
#'
#' @param testData data.frame of test data with traits in columns. 
#' @param ontology data.frame with two columns, first must be the trait pattern to match and the second is the ontology 
#' @param exactMatch logical. match the trait pattern exactly. Default is FALSE
#' @param returnDF logical. should a dataframe be returned? Default is FALSE
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
addOntology <- function(testData, ontology, exactMatch = FALSE, returnDF = FALSE){
	if(class(testData) == "data.frame"){
		wasDf <- TRUE
		if(!"trialName" %in% names(testData)) stop("'trialName' not found in 'testData'")
		testData <- list(testData)
		testNames <- unique(gsub("_.*", "", testData[[1]]$trialName))
		if(length(testNames) > 1) testData <- split(testData[[1]], gsub("_.*", "", testData[[1]]$trialName))
		names(testData) <- testNames
	} else {
		wasDf <- FALSE
	}
	for(i in names(testData)){
		for(j in 1:nrow(ontology)){
			if (exactMatch){
				for(k in ontology[[1]]){
					names(testData[[i]])[names(testData[[i]]) == k] <- ontology[[2]][ontology[[1]] == k]
				}
			} else {
				matches <- grepl(ontology$pattern[j], names(testData[[i]]))
				if(!ontology$ontology[j] %in% names(testData[[i]]) & sum(matches) == 1){				
					names(testData[[i]])[grep(ontology$pattern[j], names(testData[[i]]))] <- ontology$ontology[j]
				} else if (sum(matches) > 1) {
					stop(paste0("Multiple traits matched pattern '", ontology$pattern[j], "'"))
				}
			}
		}
	}
	if(wasDf & returnDF) testData <- do.call(rbind, testData) # 
	return(testData)
}
