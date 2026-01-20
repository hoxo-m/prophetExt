#' @importFrom ggplot2 autolayer
#' @export
ggplot2::autolayer

#' @importFrom ggplot2 autolayer aes geom_point
#' @importFrom rlang .data !!!
#'
#' @export
autolayer.prophet_outlier <- function(object, ...) {
  if (nrow(object) == 0L) {
    warning("no outliers")
    return(NULL)
  }

  colour <- "red"
  dots <- rlang::dots_list(...)
  if ("col" %in% names(dots)) {
    colour <- dots$col
    dots$col <- NULL
  }
  if ("color" %in% names(dots)) {
    colour <- dots$color
    dots$color <- NULL
  }
  if ("colour" %in% names(dots)) {
    colour <- dots$colour
    dots$colour <- NULL
  }
  geom_point(aes(.data$ds, .data$y), data = object, stat = "identity",
             position = "identity", !!!dots, colour = colour)
}
