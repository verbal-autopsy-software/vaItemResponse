#' Histograms for the item response patterns.
#'
#' \code{plotItems} is intended to be used with the Items data frame produced
#' by \code{itemMissing}.
#'
#' @param items A data frame returned from \code{itemMissing}.
#' @param response Name of the column in the data frame to plot.
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
        stop(paste0(response_name, " is not a column in ", items_name))
    }
    x <- items[, response]

    g <- ggplot(data = items, aes_string(x = args[[3]]), na.rm = TRUE) +
        geom_histogram() +
        labs(x = args[[3]], y = "# of items") +
        geom_vline(aes(xintercept = median(x, na.rm = TRUE), color = "median"),
                   linetype = "dashed", size = 1.1) +
        geom_vline(aes(xintercept = quantile(x, probs = .75, na.rm = TRUE),
                   color = "pct75"), linetype = "dashed", size = 1.1) +
        geom_vline(aes(xintercept = quantile(x, probs = .99, na.rm = TRUE),
                   color = "pct99"), linetype = "dashed", size = 1.15) +
        scale_color_manual(name = "",
                           values = c(median = "blue", pct75 = "purple", pct99 = "red"),
                           labels = c("median", "75th percentile", "99th percentile"))

    suppressWarnings(print(g))

}
