#' mergeHarvM function
#'
#' function to (do something)
#'
#' @param hmL [value]
#' @param sclL [value]
#' @param djL [value]
#' @param consensus Should a consensus be used to merge moisture and test weight data from dickey john and harvest master?
#' @param keepRangeRow Should Range and Row from Harvest Master be kept? Useful if trials dont have a trialDesign
#' @param keepNotes Should Notes from teh harvest master be kept?
#' @param rmPlotPattern regex pattern to remove plots, such as fill plots. 
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
mergeHarvM <- function(hmL, sclL, djL, consensus = FALSE, keepRangeRow = TRUE, keepNotes = TRUE, rmPlotPattern = NULL, ...){
	# consensus = TRUE; keepRangeRow = TRUE; keepNotes = TRUE; rmPlotPattern = NULL
	rp <- NULL
	if(keepRangeRow) rp <- c(rp, "RangeHM", "RowHM") 
	if(keepNotes) rp <- c(rp, "NotesHM") 
	for (i in names(hmL)) {
		# i = "WinterFacultative_2023_WARVA"
		test <- gsub("_.*", "", i)
		if(test %in% names(sclL)){
			whichTrial <- grep(i, names(sclL[[test]]))
			if (length(whichTrial)) {
				message("Plot scale data already exists for ", i, " with Harvest Master data. Creating new variable 'netWeight_HarvM', and merging...")
				# rename <- which(names(hmL[[i]]) %in% c("Moisture", "netWeight", "TestWeight"))
				rename <- which(names(hmL[[i]]) %in% c("netWeight"))
				names(hmL[[i]])[rename] <- paste0(names(hmL[[i]])[rename], "_HarvM")
				sclL[[test]][[whichTrial]] <- merge(sclL[[test]][[whichTrial]], hmL[[i]][c("blockName", "plot_name", "netWeight_HarvM", rp)], by = "plot_name", all = TRUE)
				if(consensus){
					sclL[[test]][[whichTrial]] <- consensusYld(sclL[[test]][[whichTrial]], ...)
				}
			} else {
				message("Adding Harvest Master Scale data for trial: ", i)
				sclL[[test]][[i]] <- hmL[[i]][c("blockName", "plot_name", "netWeight", rp)]
			}
		} else {
			message("Adding Harvest Master Scale data for trial: ", i)
			sclL[[test]][[i]] <- hmL[[i]][c("blockName", "plot_name", "netWeight", rp)]
		}
		if(test %in% names(djL)){
			whichTrial <- grep(i, names(djL[[test]]))
			if (length(whichTrial)) {
				message("Test weight & Moisture data already exist for ", i, " with Harvest Master data. Creating new variables 'TestWeight_HarvM' and 'Moisture_HarvM', and merging...")
				rename <- which(names(hmL[[i]]) %in% c("Moisture", "TestWeight"))
				names(hmL[[i]])[rename] <- paste0(names(hmL[[i]])[rename], "_HarvM")
				djL[[test]][[whichTrial]] <- merge(djL[[test]][[whichTrial]], hmL[[i]][c("blockName", "plot_name", "TestWeight_HarvM", "Moisture_HarvM")], by = "plot_name", all = TRUE)
				if(consensus){
					djL[[test]][[whichTrial]] <- consensusTW(djL[[test]][[whichTrial]], ...)
				}
			} else {
				message("Adding Harvest Master TestWeight and Moisture data for trial: ", i)
				djL[[test]][[i]] <- hmL[[i]][c("blockName", "plot_name", "TestWeight", "Moisture")]
			}
		} else {
			message("Adding Harvest Master TestWeight and Moisture data for trial: ", i)
			djL[[test]][[i]] <- hmL[[i]][c("blockName", "plot_name", "TestWeight", "Moisture")]
		}
	}
	return(list(dj = djL, scl = sclL))
}
