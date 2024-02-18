#' inchToCm function
#'
#' function to convert inches to centimeters.
#'
#' @param inches [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
inchToCm <- function(inches){
	# 2.54 cm per inch
	inches * 2.54
}

#' cmToInch function
#'
#' function to convert centimeters to inches.
#'
#' @param cm [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
cmToInch <- function(cm){
	# 2.54 cm per inch
	cm / 2.54
}

#' lbbutogl function
#'
#' function to convert testweigth from pounds / bushel to grams / liter
#'
#' @param lbbu [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
lbbutogl <- function(lbbu){
	# lb / bu * 1 bu / 35.2391 l * 453.592 g / 1 lb
	# 35.2391 liters per bushel
	# 453.592 g per lb
	lbbu / 35.2391 * 453.592
}

#' gltolbbu function
#'
#' function to convert testweigth from grams / liter to pounds / bushel 
#'
#' @param gl [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
gltolbbu <- function(gl){
	# 35.2391 liters per bushel
	# 453.592 g per lb
	gl * 35.2391 / 453.592
}



#' awnsToScore function
#'
#' function to convert awns A, TA, AL) to an awn score (1, 2, 3) 
#'
#' @param awns [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
awnsToScore <- function(awns){
	if(!all(awns[!is.na(awns)] %in% c("A", "TA", "AL", "M", "Mix", "Mixed", "S", "Seg", "m", "mix", "mixed", "s", "seg" ))) stop("Only valid values are 'A' for Awns, 'TA' for tip-awned, and 'AL' for awnless")
	awnScore <- rep(NA, length(awns))
	awnScore[awns == "A"] <- 3
	awnScore[awns == "TA"] <- 2
	awnScore[awns == "AL"] <- 1
	awnScore[awns %in% c("M", "Mix", "Mixed", "S", "Seg", "m", "mix", "mixed", "s", "seg")] <- 4
	return(awnScore)
}

#' scoreToAwns function
#'
#' function to convert awn scores (1, 2, 3) to awns (A, TA, AL) 
#'
#' @param awnScore [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
scoreToAwns <- function(awnScore){
	if(!all(awnScore %in% 1:3)) stop("Only valid values are '3' for Awns, '2' for tip-awned, and '1' for awnless")
	awns <- rep(NA, length(awnScore))
	awns[awnScore == 3] <- "A"
	awns[awnScore == 2] <- "TA"
	awns[awnScore == 1] <- "AL"
	return(awnScore)
}

#' sqftToHectare function
#'
#' function to convert square feet to hectares
#'
#' @param sqft [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
sqftToHectare <- function(sqft){
	# 0.092903 meters per square foot
	sqft * 0.09290304 / 1e4
}

#' sqftToAcre function
#'
#' function to convert square feet to acres
#'
#' @param sqft [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
sqftToAcre <- function(sqft){
	# 43560 sqft per acre
	sqft / 43560
}

#' buToKg function
#'
#' function to convert bushels to kilograms based on standard testweight (60 for wheat and 48 for barley)
#'
#' @param bu numeric. Bushels.
#' @param barley logical. Is it barley? 
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
buToKg <- function(bu, barley = FALSE){
	if(barley) {
		lbs <- bu * 48
	} else {
		lbs <- bu * 60 
	}
	# 0.45359237 kg / lb
	lbs * 0.45359237
}

#' kgToBu function
#'
#' function to convert bushels to kilograms based on standard testweight (60 for wheat and 48 for barley)
#'
#' @param kg numeric. Bushels.
#' @param barley logical. Is it barley? 
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
kgToBu <- function(kg, barley = FALSE){
	# 0.45359237 kg / lb = 2.204623 lb / kg
	lbs <- kg / 0.45359237
	if(barley) {
		bu <- lbs / 48
	} else {
		bu <- lbs / 60 
	}
	return(bu)
}

#' buacToKgha function
#'
#' function to convert bushels per acre to kilograms per hectare based on standard testweight (60 for wheat and 48 for barley)
#'
#' @param bu numeric. Bushels.
#' @param barley logical. Is it barley? 
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
buacToKgha <- function(buac, barley = FALSE){
	if(barley) {
		lbsac <- buac * 48
	} else {
		lbsac <- buac * 60 
	}
	# 0.45359237 lbs per kilo
	kgac <- lbsac * 0.45359237
	# 2.47105 acres per hectare
	kgac * 2.47105
}

#' buacToLbkg function
#'
#' function to convert kilograms per hectare to bushels per acre based on standard testweight (60 for wheat and 48 for barley)
#'
#' @param bu numeric. Bushels.
#' @param barley logical. Is it barley? 
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
kghaToBuac <- function(kgha, barley = FALSE){
	# 0.45359237 lbs per kilo
	# 2.47105 acres per hectare
	lbsha <- kgha / 0.45359237
	lbsac <- lbsha / 2.47105
	if(barley) {
		buac <- lbsac / 48
	} else {
		buac <- lbsac / 60 
	}
	return(buac)
}


#' correctWeight function
#'
#' function to correct weight based on percent moisture. Standard moisture is 13.5 for wheat and 14.5 for barley
#'
#' @param weight [value]
#' @param moisture [value]
#' @param stdMoisture numeric. moisture percentage to standardize grain yield.  
#' @param barley logical. Is it barley? 
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
correctWeight <- function(weight, moisture, stdMoisture = 13.5, barley = FALSE, ...){
 	if (barley) {
 		stdMoisture <- 14.5
 	}
 	ml1 <- moisture < 1
 	ml1[is.na(ml1)] <- FALSE
	if(any(ml1)) warning("expecting moisture as a percent between 0 and 100, not between 0 and 1")
	ratio <- (1 - 0.01 * moisture) / (1 - 0.01 * stdMoisture)
	corWeight <- weight * ratio
}

#' lbsToYield function
#'
#' function to convert pounds to grain yield using standard moisture
#'
#' @param weight numeric. grain weight in pounds. 
#' @param moisture numeric. grain moisture as a percent, 0 < moisture < 100
#' @param sqft numeric. plot area in square feet. 
#' @param barley logical. Is it barley? 
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
lbsToYield <- function(lbs, moisture, sqft, barley = FALSE, ...){ # verified works
	 if (barley) {
 		stdBu <- 48
 	} else {
 		stdBu <- 60
 	}
	lbs <- correctWeight(lbs, moisture, barley = barley,...) 
	bu <- lbs / stdBu
	bu / sqftToAcre(sqft)
}


#' lbsToYield function
#'
#' function to convert pounds to grain yield using standard moisture
#'
#' @param g numeric. Grain mass in grams.
#' @param moisture numeric. grain moisture as a percent, 0 < moisture < 100
#' @param sqft numeric. plot area in square feet. 
#' @param sqm numeric. plot area in square meters. 
#' @param metric logical. should metric units be returned?
#' @param barley logical. Is it barley? 
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
gramsToYield <- function(g, moisture, sqft = NULL, sqm = NULL, metric = FALSE, barley = FALSE, ...){
	if(is.null(sqft) & is.null(sqm)){
		stop("sqft or sqm of plot area must be provided!")
	}

	g <- correctWeight(g, moisture, ...) # output in g
	
	if(is.null(sqm)){
		sqm <- sqft / 10.76391
	} else {
		sqft <- sqm * 10.76391
	}
	
	if(metric){
		kg <- g / 1e3 
		yld <- kg / sqm * 1e4 # kg ha-1
	} else{
		# 453.592 g / lb
		if (barley) {
	 		stdBu <- 48
	 	} else {
	 		stdBu <- 60
	 	}
		lbs <- g / 453.59237
		bu <- lbs / stdBu
		yld <- bu / sqft * 43560
	}
	return(yld)
}

# lbsToYield(lbs = 6.76, moisture = 14.75, sqft = 45)
# gramsToYield(g = 3066.284, moisture = 14.75, sqft = 45)
# gramsToYield(g = 3066.284, moisture = 14.75, sqm = 4.18064)



