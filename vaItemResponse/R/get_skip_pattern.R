#' Create skip pattern list to account for fixed symptom dependence 
#' 
#' @param data data frame of the WHO2016 data in the InterVA-5 input format.
#' @param COD data frame with ID and cause of death.
#' @param verbose whether to print the symptom tree structures for diagnostics.
#' @param remove.injury whether to remove injury related symptoms
#' @return a list of binary data, symptom table, and dependence structure.
#' @examples
#' library(InterVA5)
#' data(RandomVA5)
#' random_COD <- data.frame(ID = RandomVA5$ID, cause = "Unknown")
#' data <- organize_dependence(RandomVA5, COD = random_COD)
#' head(data$symptoms)
#' head(data$causes)
#' head(data$ID)
#' summary(data$dependence)
#' @export
#' 
organize_dependence <- function(data, COD, verbose=FALSE, remove.injury = TRUE){
	data("probbaseV5", envir = environment())
	probbaseV5 <- get("probbaseV5", envir  = environment())
	N <- dim(data)[1]
 
 	data <- data[data[, 1] %in% COD[, 1], ]
 	COD <- COD[match(data[, 1], COD[, 1]), ]


	data0 <- matrix(0, dim(data)[1], dim(data)[2])
	for(i in 1:dim(data0)[2]){
		data0[which(tolower(data[,i]) == "y"), i] <- 1
		# data0[which(data[,i] == "-"), i] <- 0.5
	}
	colnames(data0) <- colnames(data)
	ID = as.character(data[, 1])
	data0 <- data0[, -1]
	
	symps_name <- as.matrix(probbaseV5[, c(1:3)])[-1, ]
	symps_name[which(symps_name[,1] == "i289o"), 3] <- "yellow skin"
	symps_name[which(symps_name[,1] == "i277o"), 3] <- "stiff baby"
	symps_name[which(symps_name[,1] == "i382a"), 3] <- "lab delivery 24+h" 
	symps_cond <- as.matrix(probbaseV5[, c(1, 6, 7:16)])[-1, ]

	if(remove.injury){
		inj <- probbaseV5[21:39, 1]
		data0 <- data0[, colnames(data0) %in% inj == FALSE]
		symps_name <- symps_name[symps_name[, 1] %in% inj == FALSE, ]	
		symps0 <- symps_name[match(colnames(data0), symps_name[,1]), 3]
		symps_cond <- symps_cond[symps_cond[, 1] %in% inj == FALSE, ]		
	}

	S <- dim(data0)[2] 
	detN <- vector("list", S)
	detY <- vector("list", S)
	detCh <- vector("list", S)

	get_level <- function(x){as.numeric(substr(x, 2, 4))}
	# for(i in 1:dim(symps_cond)[1]){
	# 	x <- get_level(symps_cond[i, 1])
	# 	y <- get_level(symps_cond[i, 4:12])
	# 	if(sum(!is.na(y)) == 0) print(c(i, x, symps_cond[i, 1:12]))
	# 	if(max(y, na.rm =TRUE) > x) {
	# 		ch <- symps_name[which(symps_name[, 1] == symps_cond[i, 1])	, 2]
	# 		for(j in which(y > x)){
	# 			pa <- symps_name[which(symps_name[, 1] == substr(symps_cond[i, 3 + j], 1, 5))	, 2]
	# 			print(paste0(i, pa, " ----->  ", ch))
	# 		}
	# 	}
	# }
	
	# reverse the order to make the parent nodes from top to bottom when parallel nodes exist
	for(i in S:1){
		# Handle Don't ask
		tmp <- as.character(symps_cond[i, 4:11])
		tmp <- tmp[tmp != ""]
		# sign <- symps_cond[i, 2] == "Y"
		signtmp <- substr(tmp, 6,6)
		tmp <- substr(tmp, 1, 5)
		tmp <- match(tmp, colnames(data0))
		tmp <- tmp[!is.na(tmp)]
		if(length(tmp) > 0){
			for(j in 1:length(tmp)){
				level_i <- get_level(symps_name[i, 1])
				level_j <- get_level(symps_name[tmp[j], 1])

				# if the condition is the same or further level, only add to detCh (mutual)
				if(level_i <= level_j){
					detCh[[tmp[j]]] <- c(detCh[[tmp[j]]], i)	
					detCh[[i]] <- c(detCh[[i]], tmp[j])	
					next
				}
				# if the condition is at earlier level
				# then i is a child of j
				if(level_i > level_j){
					# if tmp[j] is signtmp[j] then don't ask item i
					detCh[[tmp[j]]] <- c(detCh[[tmp[j]]], i)	
					# if    signtmp[j] == Y 
					# then  item i is only asked if tmp[j] = N
					# i.e.  item i implies tmp[j] = N
					if(signtmp[j] == "Y"){
						detN[[i]] <- c(detN[[i]], tmp[j])
					}else{
						detY[[i]] <- c(detY[[i]], tmp[j])
					}
				}
			}
		}

	
		# Handle Ask if
		tmp <- as.character(symps_cond[i, 12])
		tmp <- tmp[tmp != ""]
		# sign <- symps_cond[i, 2] == "Y"
		signtmp <- substr(tmp, 6,6)
		tmp <- substr(tmp, 1, 5)
		tmp <- match(tmp, colnames(data0))
		tmp <- tmp[!is.na(tmp)]
		if(length(tmp) > 0){
			for(j in 1:length(tmp)){
				level_i <- get_level(symps_name[i, 1])
				level_j <- get_level(symps_name[tmp[j], 1])
				# if the condition is the same or further level, only add to detCh (mutual)
				if(level_i <= level_j){
					detCh[[tmp[j]]] <- c(detCh[[tmp[j]]], i)	
					detCh[[i]] <- c(detCh[[i]], tmp[j])	
					next
				}
				# if the condition is at earlier level
				# then i is a child of j
				if(level_i > level_j){
					# if tmp[j] is signtmp[j] then don't ask item i
					detCh[[tmp[j]]] <- c(detCh[[tmp[j]]], i)	
					# if    signtmp[j] == Y 
					# then  item i is only asked if tmp[j] = N
					# i.e.  item i implies tmp[j] = N
					if(signtmp[j] == "Y"){
						detY[[i]] <- c(detY[[i]], tmp[j])
					}else{
						detN[[i]] <- c(detN[[i]], tmp[j])
					}
				}
			}
		}		
	}
	for(i in 1:S){
		if(!is.null(detN[[i]])) detN[[i]] <- unique(detN[[i]])	
		if(!is.null(detY[[i]]))detY[[i]] <- unique(detY[[i]])	
		if(!is.null(detCh[[i]]))detCh[[i]] <- unique(detCh[[i]])	
	}

	if(verbose){
		# Check the conditions
		for(i in 1:S){
			if(!is.null(detCh[[i]])){
				message(paste0(symps0[i], " ----> "))
				for(j in detCh[[i]]){
					message(paste("      ", symps0[j]))	
				}
			}
		}


		# Check the conditions
		for(i in 1:S){
			if(!is.null(detN[[i]])){
				message(paste0(symps0[i], " <X---- "))
				for(j in detN[[i]]){
					message(paste("      ", symps0[j]))	
				}
			}
			if(!is.null(detY[[i]])){
				message(paste0(symps0[i], " <---- "))
				for(j in detY[[i]]){
					message(paste("      ", symps0[j]))	
				}
			}
		}
	}

	out <- list(data = data0, ID = ID,
				dependence = list(detY = detY, detN = detN, detCh = detCh), 
				symptoms = symps_name, 
				causes = COD)
	return(out)
}