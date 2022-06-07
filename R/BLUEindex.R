#' roundSummary function
#'
#' function to round summaries. Only works if plus minus present 
#'
#' @param envMeans matrix of environment means
#' @param cv coefficicent of variation  
#' @param weights weights for each environment. default is 1/ncol(envMeans)
#' @param scaleb should the index be scaled to 1? default is TRUE. 
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
BLUEindex <- function(envMeans, cv, weights = 1, scaleb = TRUE){
	BLUE <- as.matrix(envMeans)
	G <- var(BLUE)
	
	mu <- colMeans(BLUE)
	sigma <- CV/100 * mu
	R <- diag(sigma)
	P <- G + R
	if(length(weights) == 1 & weights[1] == 1){
		weights <- weights / ncol(G)
	} 
	b <- solve(P) %*% G %*% matrix(weights, nrow = ncol(G))
	b <- b / sum(b)
	envInd <- BLUE %*% b
	# rowMeans(BLUE)[order(-rowMeans(BLUE))]
	# rank(-envInd)
	envInd[order(-envInd),]
}
