#' makeSquare function
#' 
#' function to assign purdy pedigrees symbols correctly when parents are expressed as pedigrees (e.g. A/B crossed to C gives A/B//C). Needs the user to put a recognizable symbol between female ad male pedigrees, such as /x/ 
#'
#' @param ped pedigree with symbol between female and male pedigrees
#' @param dvars character vector of variables to add dummy to
#' @param range character. data.frame variable name to indicate row. Default is 'range'
#' @param pass character. data.frame variable name to indicate column. Default is 'pass'
#' @param dummy character. Stand in value for added plots to make square. Default is 'dummy' 
#' @param by character. dF column name by which to apply square, usually physically  separate locations, blocks, etc. 
#' @return data.frame with added dummy plots to make square. 
#' @details [fill in details here]
#' @examples
#' rightpedsym("A/B /x/ C")
#' rightpedsym("A/B /x/ C", includeSpace=FALSE)
#' rightpedsym("A x B/C", pattern = "x", includeSpace=FALSE)
#' rightpedsym("A x B/C", pattern = " x ", includeSpace=FALSE)
#' @export
rightpedsym <- function(ped, pattern = "\\s*/x/\\s*", includeSpace = TRUE) {
	if(includeSpace) pad <- " " else pad <- ""
	fm <- strsplit(ped, pattern)
	f <- sapply(fm, "[", 1)
	m <- sapply(fm, "[", 2)
	ftc <- grepl("\\/\\/", f)
	mtc <- grepl("\\/\\/", m)
	fc <- grepl("\\/", f)
	mc <- grepl("\\/", m)
	xsym <- rep(paste0(pad, "/", pad), length(ped)) 
	xsym[ftc | mtc] <- paste0(pad, "/3/", pad)
	xsym[{fc|mc} & !{ftc | mtc}] <- paste0(pad, "//", pad)
	newped <- paste0(f, xsym, m)
	return(newped)
}