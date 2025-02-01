#' xtable2 function
#'
#' function to print latex table with padding for easier reading / manual manipulation thna xtable (xtable may have this feature?)
#'
#' @param tab table to be printed into a latex table
#' @param aln character of length 1, indicating alignment, l, r or c. 
#' @param pad extra pad to be added
#' @param digits vector of length ncol(tab) indicating the number of digits. If columns are not nueric, you still have to add some value for each column, which will be ignored. 
#' @param include.rownames should rownames of the table be ignored? default is TRUE
#' @param formatColNames will reformat column names of the type 'Trait|unit (#trials)' to multiple lines, 'Trait \\ unit \\ #trials' for better printing
#' @param statsLine should a line be printed before mean, CV, LSD?
#' @param caption latex latex caption. If you complicated you might want to skip this and edit the output tex table 
#' @param label latex latex label name
#' @param size latex textsize. do not add slashes before, they will be appended automatically. 
#' @param naReplace character. What character should replace missing table cells. Default is "". If NA is desired in the output, set naReplace = NULL
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
xtable2 <- function(tab, aln = NULL, pad = 0,  digits = 2, caption = NULL, label = NULL, size = "footnotesize", footNotes = NULL, include.rownames = TRUE, formatColNames = TRUE, statsLine = TRUE, specialChars = c("#", "_"), naReplace = "", ...){
	# if numeric, shoudl round to something
	# tab <- usnCheckSum[c(1, 2:16)]; pad = 0; digits = 2; aln = NULL; include.rownames = FALSE; formatColNames = TRUE; statsLine = TRUE; size = "footnotesize"; specialChars = c("#"); caption = NULL; label = NULL; size = "footnotesize"; footNotes = NULL; naReplace = ""
	size <- paste0("\\", size)

	if(length(digits) == 1) {
		digits <- rep(digits, ncol(tab))
	} else if (length(digits) != ncol(tab)){
		stop("length of digits must equal 1 or ncol(tab)")
	}
	if(include.rownames){
		tab <- cbind(rownames(tab), tab)	
	}
	if(formatColNames){
		colHead <- formatXtabColNames(colnames(tab))
	} else {
		colHead <- matrix(colnames(tab), nrow = 1)
	}
	hasPerc <- grep("\\%", colHead)
	if(any(hasPerc)) {
		colHead[hasPerc] <- gsub("\\%", "\\\\%", colHead[hasPerc])
		needSpace <- sapply(gregexpr("\\\\", colHead[hasPerc]), function(x) sum(x > 0))
		colHead[hasPerc] <- paste0(colHead[hasPerc], sapply(needSpace, function(x) paste0(rep(" ", x), collaspe = "")))
	}
	nrowColHead <- nrow(colHead) 
	classx <- apply(tab, 2, class)
	whichNum <- which(sapply(tab, is.numeric))
	if(is.null(aln)){
		colTypes <- rep("l", ncol(tab))
		colTypes[whichNum] <- "c"
		colTypes <- paste0(colTypes, collapse = "")
	} else {
		if(nchar(aln) != ncol(tab)) stop("nchar(aln) must equal the number of columns of tab!")
	}
	if(!is.null(naReplace)){
		tab[is.na(tab)] <- naReplace
	}
	padTab <- matrix(NA, nrow(tab) + nrowColHead, ncol(tab))
	padType <- strsplit(colTypes, "")[[1]]
	for(j in 1:ncol(tab)){

		if(is.numeric(tab[, j])) tab[, j] <- round(tab[, j], digits = digits[j])
		# if(is.null(padType[j]) | is.na(padType[j])) {print(j); browser()}
		tabj <- as.character(tab[, j])
		for(i in specialChars) {
			tabj <- gsub(paste0("\\", i), paste0("\\\\", i), tabj)
		}
		padTab[, j] <-  strPad(c(colHead[, j], tabj), type = padType[j], pad = pad)
	}
	# padTab <- apply(tab, 2, strPad, type = strsplit(colTypes, ""))

	xtabLines <- paste0(apply(padTab, 1, paste0, collapse = " & "), " \\\\")

	header <- c("\\begin{table}[ht]")
	if(!is.null(caption)){
		header <- c(header, paste0("\\caption{", caption, "}"))
	}
	if(!is.null(label)){
		header <- c(header, paste0("\\label{", label, "}"))
	}
	header <- c(header, "\\centering",
				size, 
				paste0("\\begin{tabular}{", colTypes, "}"), 
				"\\hline")

	trailer <- c("\\hline", 
				 "\\end{tabular}")
	if(!is.null(footNotes)){
		footNotes[1:{length(footNotes)-1}] <- paste0(footNotes[1:{length(footNotes)-1}], "\\\\")
		trailer <- c(trailer, 
			        "\\raggedright{",
					"\\footnotesize",
					footNotes,
					"}") 
	} 
	trailer <- c(trailer, "\\end{table}")
	

	columnHeader <- xtabLines[1:nrowColHead]
	if(statsLine){
		meanLSDCV <- grep("^mean$|^L\\.*S\\.*D$|C\\.*V", tab[,1], ignore.case = TRUE)
		if(length(meanLSDCV)){
			lastDataLine <- min(meanLSDCV) + nrowColHead -1
			trailer <- c("\\hline", xtabLines[{lastDataLine + 1}:length(xtabLines)], trailer)
		} else {
			lastDataLine <- length(xtabLines)
		}
	} else {
		lastDataLine <- length(xtabLines)
	}
	
	body <- xtabLines[{nrowColHead + 1}:lastDataLine]

	allXtabLines <- c(header, columnHeader, "\\hline", body, trailer)
	# writeLines(allXtabLines)
	writeLines(allXtabLines, ...)
	# if l then  lpad then , if c hen cpad, if r then rpad
}
