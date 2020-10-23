#' Map ICD-10 codes into the WHO cause list
#' 
#' @param metric a data frame of item significance metric with at least the following three columns: cause, symptom, value. This can be the combined output from evaluate_symptom function.
#' @param data output of the organize_dependence function. 
#' @examples
#' \dontrun{
#' require(InterVA5)
#' data(RandomVA5)
#' random_COD <- data.frame(ID = RandomVA5$ID)
#' set.seed(1)
#' random_COD$cause = sample(c("Cause 1", "Cause 2"), 
#' 							dim(random_COD)[1], replace=TRUE)
#' data <- organize_dependence(RandomVA5, COD = random_COD)
#' metric_all <- NULL
#' for(s in  data$symptoms[1:10, 1]){
#'	 metric <- evaluate_symptom(data = data, symptom = s)
#'   metric_all <- rbind(metric_all, metric)
#' }
#' summary <- summary_ranking(metric_all, data)
#' summary$symptom[["i022a"]]
#' summary$table
#' summary$percentile
#' }
#' @export
#' 
summary_ranking <- function(metric, data){
	getlist <- function(x, id){
			xx <- subset(x, symptom == id)
			causes <- as.character(xx$cause[!is.na(xx$value)])
			if(length(causes) == 0) causes <- NA
			max_effect <- xx$value[which.max(abs(xx$value))]
			if(length(max_effect) == 0) max_effect <- NA
			max_cause <- as.character(xx$cause[which.max(abs(xx$value))])
			if(length(max_cause) == 0) max_cause <- NA

			out <- list(
				# id
				ID = as.character(id),
				# text
				name = xx$symptom_text[1],
				# max value
				max_absolute_effect = max_effect,
				# most related cause
				max_absolute_cause = max_cause, 
				calculated_causes = causes
				)
			return(out)
	}

	symps <- unique(metric$symptom)
	S <- length(symps)
	sympslist <- vector("list", S)
	for(i in 1:S){
		names(sympslist)[i] <- data$symptoms[which(data$symptoms[,1] == symps[i]), 1]
	}
	metric$symptom_text <- data$symptoms[match(metric$symptom, data$symptoms[,1]), 2]

	symps_name <- data$symptoms
    symps_name[,2] <- paste0(symps_name[,1], ": ", symps_name[,2])
	causelist <- as.character(unique(data$causes[,2]))
	C <- length(causelist)


	m <- NULL
	for(i in 1:dim(symps_name)[1]){
		which <- which(names(sympslist) == symps_name[i, 1])
		if(length(which) == 0){
			warning(symps_name[i, 1], " not exist in the results")
			next
		}
		sympslist[[which]] <- getlist(metric, symps_name[i, 1])
		m <- c(m, sympslist[[which]]$max_absolute_effect)
	}
	
	# add percentile, lower is more important
	pp <- NULL
	for(i in 1:length(sympslist)){
		sympslist[[i]]$percentile <- sum(abs(m) >= abs(sympslist[[i]]$max_absolute_effect), na.rm = TRUE) / sum(!is.na(m))
		if(is.na(sympslist[[i]]$max_absolute_effect)) sympslist[[i]]$percentile <- NA
		pp <- c(pp, sympslist[[i]]$percentile)
	}
	names(pp) <- names(sympslist)
	pp <- sort(pp, decreasing=FALSE)
	pp <- data.frame(symptom = names(pp), percentile = as.numeric(pp))

	sympstable <- matrix(NA, S, C)
	rownames(sympstable) <- symps
	colnames(sympstable) <- causelist
	for(i in 1:S){
		for(j in 1:C){
			tmp <- subset(metric, symptom == symps[i] & cause == causelist[j])
			sympstable[i, j] <- ifelse(length(tmp)==0, NA, tmp$value)
		}
	}

	out <- list(symptom = sympslist, 
				table = sympstable, 
				percentile = pp )

	return(out)
}