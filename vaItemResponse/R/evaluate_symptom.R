#' Create skip pattern list to account for fixed symptom dependence 
#' 
#' @param data data frame of the WHO2016 data in the InterVA-5 input format.
#' @param COD data frame with ID and cause of death.
#' @param verbose whether to print the symptom tree structures for diagnostics.
#' @param remove.injury whether to remove injury related symptoms
#' @return a list of binary data, symptom table, and dependence structure.
#' @examples
#' require(InterVA5)
#' data(RandomVA5)
#' random_COD <- data.frame(ID = RandomVA5$ID)
#' set.seed(1)
#' random_COD$cause = sample(c("Cause 1", "Cause 2"), 
#' 							dim(random_COD)[1], replace=TRUE)
#' data <- organize_dependence(RandomVA5, COD = random_COD)
#' metric <- evaluate_symptom(data = data, symptom = "i147o")
#' metric
#' @export
#' @import tmle 
#' @import glmnet 
#' @import randomForest
#'  
evaluate_symptom <- function(data, symptom, causes.compute=NULL){
	require(tmle)
	require(glmnet)
	require(randomForest)

	if(!is.list(data)) stop("data argument should be a list returned from organize_dependence() function.")

		detY <- data$detY
		detN <- data$detN
		detCh <- data$detCh
		causelist <- unique(data$causes[, 2])
		slist <- data$symptoms[, 1]

		W <- data$data
		Y <- data$causes[, 2]
		k <- which(slist == symptom)
		if(length(k) == 0){
			stop("Invalid symptom input. Need to be one of the column names of the VA data.")
		}

		metric.all <- NULL
		print("Start calculating symptom importance...")
		
		if(!is.null(causes.compute)) causelist <- causes.compute
		
		causelist <- sort(as.character(causelist))
		
		for(c in 1:length(causelist)){
			tt <- Sys.time()

			A <- W[, k]
			remove <- k

			remain <- which(A %in% c(0, 1))
			if(!is.null(detY[[k]])){
				for(kk in detY[[k]]) remain = intersect(remain, which(W[,kk] == 1))
				remove <- c(remove, detY[[k]])	
			}
			if(!is.null(detN[[k]])){
				for(kk in detN[[k]]) remain = intersect(remain, which(W[,kk] == 0))
				remove <- c(remove, detN[[k]])
			}
			remove <- unique(c(remove, detCh[[k]]))
			outcome <- rep(0, length(Y[remain]))
			outcome[Y[remain] == causelist[c]] <- 1

			skip <- FALSE
	        if(length(remain) < 2){ msg <- "No data left"; skip = TRUE}
	        if(min(sum(outcome == 0), sum(outcome==1)) < 10){ msg <- "No variation in outcome"; skip = TRUE}
	        if(min(sum(A[remain] == 0), sum(A[remain]==1)) < 10){ msg <- "No variation in symptom"; skip = TRUE}
	        if(skip){
	        	metric <-  data.frame(cause=causelist[c], symptom = slist[k], value = NA, lower=NA, upper=NA)
	        }else{
				X <- data.frame(cbind(outcome, A[remain], W[remain, -remove]))
				colnames(X)[1] <- "Y"
				# print(dim(X))
			    # print(table(X$Y))
			    f <- try(
					tmle(as.numeric(outcome), A[remain], W[remain, -c(remove)], 
					family = "binomial", 
					Q.SL.library = c("SL.glmnet", "SL.randomForest"),
					g.SL.library = c("SL.glmnet"),
					gbound = 0.025),
					# list(estimates = list(ATE = list(psi = 1))), 
					TRUE)
				if(is(f, "try-error")){
					f <- NULL
				} 
				if(is.null(f)){
					metric <-  data.frame(cause=causelist[c], symptom = slist[k], value = NA, lower=NA, upper=NA)
				}else{
					metric <-  data.frame(cause=causelist[c], symptom = slist[k], value = f$estimates$ATE$psi, lower = f$estimates$ATE$CI[1], upper = f$estimates$ATE$CI[2])
				}

	        }

	        metric.all <- rbind(metric.all, metric)
	        if(skip){
		        message(causelist[c], " skipped: \n    ", msg)

	        }else{
		        message(causelist[c], " evaluated: \n    ", appendLF = FALSE)
		        print(Sys.time() - tt)	        	
	        }
	    }

		return(metric.all)	
}