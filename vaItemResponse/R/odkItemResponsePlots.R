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
#' results <- itemMissing(who2016_151_data, who2016_151_instrument)
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


#' Heatmap for selected response items.
#'
#' \code{heatMap} is intended to be used with the Items data frame produced
#' by \code{itemMissing}.
#'
#' @param items A data frame returned from \code{itemMissing}.
#' @param first Index for the first item to include in the plot.
#' @param last Index for the last item to include in the plot.
#' @param label_length Number of characters in label per line.
#'
#' @examples
#' \dontrun{
#' ## Example with 2016 WHO VA instrument version 1.5.1
#' library(vaItemResponse)
#' data(who2016_151_data)
#' data(who2016_151_instrument)
#' results <- itemMissing(who2016_151_data, who2016_151_instrument)
#' }
#'
#' @import ggplot2 dplyr
#' @importFrom reshape2 melt
#' @importFrom stringr str_wrap
#' @export
#'
heatMap <- function (items, first = 1, last = 30, label_length = 150) {

    items$label[items$label == ""] <- items$name[items$label == ""]
    item_map <- items %>%
        slice(first:last) %>%
        select("label", pct_total, pct_ref, pct_dk, pct_miss, pct_yes, pct_no) %>%
        melt(id.vars = "label")

    item_map$variable <- recode_factor(item_map$variable,
                                       pct_total = "% asked", 
                                       pct_ref = "% refuse",
                                       pct_dk = "% don't know", 
                                       pct_miss = "% missing", 
                                       pct_yes = "% yes", 
                                       pct_no = "% no")

    g <-  item_map %>%
        ggplot(aes(variable, reorder(label, desc(label)), fill = value)) + 
        geom_tile() + 
        scale_fill_gradient2(low="blue", high="#d7191c") + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 0),
              axis.text.y = element_text(size = 10),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_rect(fill="gray50")) + 
        labs(x = "", y = "") +
        scale_x_discrete(position = "top") +
        scale_y_discrete(labels = function (x) str_wrap(x, width = label_length))

    print(g)

}
