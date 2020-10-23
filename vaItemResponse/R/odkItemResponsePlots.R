#' Histograms for the 
#'
#' \code{plotItems} 
#'
#' @param items A data frame returned from \code{itemMissing}.
#' @param response Name of the column in the data frame to plot.
#'
#' @details
#' This is a worker function.
#' 
#' @examples
#' \dontrun{
#' ## Example with 2016 WHO VA instrument version 1.5.1
#' library(vaItemResponse)
#' data(who2016_151_data)
#' data(who2016_151_instrument)
#' results <- itemMissing(who2016_151_data, who2016_151_instrument, add_percent = TRUE)
#' 
#' }
#'
#' @import ggplot2
#' @export
#'

plotItems <- function (items, response) {

    args <- as.list(sys.call())
    args[[3]] <- as.character(args[[3]])
    items_name <- deparse(substitute(items))
    response_name <- deparse(substitute(response))

    if (!(args[[3]] %in% names(items))) {
        #stop(paste0(args[[3]], " is not a column in ", as.character(args[[2]])))
        stop(paste0(response_name, " is not a column in ", items_name))
    }

    g <- ggplot(data = items, aes_string(x = args[[3]]), na.rm = TRUE) +
        geom_histogram()##  +
        ## labs(x = args[[3]], y = "# of items") +
        ## geom_vline(aes(xintercept = median(response, na.rm = TRUE)), color = "blue",
        ##            linetype = "dashed", size = 1.2) +
        ## geom_vline(aes(xintercept = quantile(response, probs = .75, na.rm = TRUE)),
        ##            color = "purple", linetype = "dashed", size = 1.2) +
        ## geom_vline(aes(xintercept = quantile(response, probs = .99, na.rm = TRUE)),
        ##            color = "red", linetype = "dashed", size = 1.72) +
        ## annotate("text", x = 75, y = 250, label = "median ----",
        ##          color = "blue", size = 8) + 
        ## annotate("text", x = 75, y = 230, label = "75th pctl ----",
        ##          color = "purple", size = 8) + 
        ## annotate("text", x = 75, y = 210, label = "99th pctl ----",
        ##          color = "red", size = 8) +
        ## theme(axis.text.x = element_text(size = 15),
        ##       axis.title.x = element_text(size = 20), 
        ##       axis.text.y = element_text(size = 15),
        ##       axis.title.y = element_text(size = 20))

    suppressWarnings(print(g))

}
## names(results$Items)
## results$Items$pct_ref
## plotItems(results$Items, "pct_yes")
