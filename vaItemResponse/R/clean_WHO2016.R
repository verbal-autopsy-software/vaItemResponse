#' Clean and check data collected using WHO 2016 questionnaire 
#' 
#' @param vadata data frame of the WHO2016 data in the InterVA-5 input format.
#' @return a data frame of symptoms after checking for inconsistencies using the InterVA-5 logic.  
#' @examples
#' require(InterVA5)
#' data(RandomVA5)
#' cleaned <- check_data(RandomVA5)
#' cleaned[1:10, 1:10]
#' @import InterVA5
#' @export
#' 
check_data <- function(vadata){
	require(InterVA5)
	# InterVA check scheme
	data.num <- matrix(0, dim(vadata)[1], dim(vadata)[2] - 1)
	for(j in 2:dim(vadata)[2]){
		data.num[which(toupper(vadata[, j]) == "Y"), j - 1] <- 1
		data.num[which(vadata[, j] %in% c("N", "n", "Y", "y") == FALSE), j - 1] <- NA
	}	
	# to be consistent with InterVA5, adding first column
	data.num <- cbind(NA, data.num)
	checked <- data.num
	warning <- vector("list", dim(vadata)[1])
	firstPass <- secondPass <- NULL
	data(probbaseV5)

	for(i in 1:dim(vadata)[1]){
		tmp <- InterVA5::DataCheck5(data.num[i,], id=vadata[i,1], probbaseV5=probbaseV5, InSilico_check = FALSE, write=FALSE)
	    checked[i, ] <- tmp$Output
	    if(i %% 10 == 0) cat(".")
	}
	checked[,1] <- vadata[,1]
	colnames(checked) <- colnames(vadata)
	checked[is.na(checked)] <- -1
	vadata2 <- ConvertData(checked, yesLabel = "1", noLabel = "0", missLabel = "-1", data.type="WHO2016")
	vadata2[, 1] <- vadata[, 1]
	return(vadata2)
}