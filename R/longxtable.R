#' longxtable function
#'
#' function to print latex table with padding for easier reading / manual manipulation thna xtable (xtable may have this feature?)
#'
#' @param tab table to be printed into a latex table
#' @param aln character of length 1, indicating alignment, l, r or c. 
#' @param pad extra pad to be added
#' @param digits vector of length ncol(tab) indicating the number of digits. If columns are not nueric, you still have to add some value for each column, which will be ignored. 
#' @param ignore.rownames should rownames of the table be ignored? default is TRUE
#' @param formatColNames will reformat column names of the type 'Trait|unit (#trials)' to multiple lines, 'Trait \\ unit \\ #trials' for better printing
#' @param statsLine should a line be printed before mean, CV, LSD?
#' @param footNotes list of nots to be added at the end of the table. 
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
longxtable <- function(tab, aln = NULL, pad = 0, digits = 2, caption = NULL, label = NULL, size = "footnotesize", footNotes = NULL, include.rownames = TRUE, formatColNames = TRUE, statsLine = TRUE, specialChars = "#", fitPageWidth = TRUE, ...){
	# if numeric, shoudl round to something
	# tab <- prmat; pad = 0; digits = 2; aln = NULL; include.rownames = FALSE; formatColNames = TRUE; statsLine = TRUE; footNotes = tabNotes; caption = "this is a caption"; label = "lab"; size = "footnotesize"; specialChars = "#"; fitPageWidth = TRUE; 
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
	lcolt <- NULL
	rcolt <- NULL
	lrborder <- NULL
	if(fitPageWidth) {
		lrborder <- c("\\setlength\\LTleft{0pt}", "\\setlength\\LTright{0pt}")
		lcolt <- "@{\\extracolsep{\\fill}}"
		rcolt <- "@{}"
	}
	if(is.null(aln)){
		# \setlength\LTleft{0pt}
		# \setlength\LTright{0pt}
		# \begin{longtable}{@{\extracolsep{\fill}}llll@{}}
		colTypes <- rep("l", ncol(tab))
		colTypes[whichNum] <- "c"
		colTypes <- paste0(colTypes, collapse = "")
	} else {
		if(nchar(aln) != ncol(tab)) stop("nchar(aln) must equal the number of columns of tab!")
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

	header <- "\\begin{ThreePartTable}"
	if(!is.null(footNotes)){
		header <- c(header, "\\begin{TableNotes}",
							  "\\footnotesize", 
							  paste0("\\item ", footNotes),
							  "\\end{TableNotes}")
	}


	columnHeader <- xtabLines[1:nrowColHead]

	header <- c(header, lrborder, paste0("\\begin{longtable}{", lcolt, colTypes, rcolt, "}"))
	if(!is.null(caption)){
		header <- c(header, paste0("\\caption{", caption, "}"))
	}
	if(!is.null(label)){
		header <- c(header, paste0("\\label{", label, "}"))
	}
	header <- c(header, "\\\\", "\\toprule%", # You might have to put a new line after the label!
				columnHeader, 
				"\\midrule%",
			    "\\endfirsthead%",
			    "\\toprule%",
				columnHeader[1:2],
				"\\midrule%",
				"\\endhead%")

	if(statsLine){
		meanLSDCV <- grep("^mean$|^L\\.*S\\.*D$|C\\.*V", tab[,1], ignore.case = TRUE)
		if(length(meanLSDCV)){
			lastDataLine <- min(meanLSDCV) + nrowColHead -1
			header <- c(header, "\\midrule%", xtabLines[{lastDataLine + 1}:length(xtabLines)], 
						"\\midrule%", 
						paste0("\\multicolumn{", ncol(tab), "}{r@{}}{continued \\ldots}\\\\"),
						"\\bottomrule",
						"\\insertTableNotes%",
						"\\endfoot%",
						"\\midrule%",
						xtabLines[{lastDataLine + 1}:length(xtabLines)],
						"\\bottomrule",
						"\\insertTableNotes%",
						"\\endlastfoot%")
		} else {
			lastDataLine <- length(xtabLines)
		}
	} else {
		lastDataLine <- length(xtabLines)
	}

	trailer <- c("\\end{longtable}",
				 "\\end{ThreePartTable}")

	body <- xtabLines[{nrowColHead + 1}:lastDataLine]

	allXtabLines <- c(header, "\\hline", body, trailer)
	# writeLines(allXtabLines)
	writeLines(allXtabLines, ...)
	# if l then  lpad then , if c hen cpad, if r then rpad
}

