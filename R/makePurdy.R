#' makePurdy function
#' 
#' function to make purdy pedigrees from female and male pedigres or line names. 
#'
#' @param female pedigree with symbol between female and male pedigrees
#' @param male character vector of variables to add dummy to
#' @return data.frame with added dummy plots to make square. 
#' @details [fill in details here]
#' @examples
#' makePurdy("A/B", "C")
#' makePurdy("A/B", "C", includeSpace = FALSE)
#' makePurdy("A/B", "C/D//E", includeSpace = FALSE)
#' @export
makePurdy <- function(female, male, includeSpace = TRUE) {
	ped <- paste0(female, " /x/ ", male)
	newped <- rightpedsym(ped, includeSpace = includeSpace)
	return(newped)
}