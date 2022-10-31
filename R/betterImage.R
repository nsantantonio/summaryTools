#' mergreNotes function
#' betterImage function
#' 
#' function to draw images in the same orientation as the matrix
#'
#' @param X matrix of values to be passed to image()
#' @return plot image in correct orientation
#' @details [fill in details here]
#' @examples betterImage(volcano)
#' @export
betterImage <- function(X, ...){
	# X <- traitMat
	X <- X[nrow(X):1, ]
	image(x = 1:ncol(X), y = 1:nrow(X), z = t(X), ...)
}
