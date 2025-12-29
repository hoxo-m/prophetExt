#' @importFrom ggplot2 autolayer
#' @export
ggplot2::autolayer

#' @importFrom ggplot2 autolayer aes geom_point
#' @importFrom rlang .data !!!
#'
#' @export
autolayer.prophet_outlier <- function(object, ...) {
  dots <- list(...)
  if ("colour" %in% names(dots)) {
    colour <- dots$colour
    dots$colour <- NULL
  } else if ("color" %in% names(dots)) {
    colour <- dots$color
    dots$color <- NULL
  } else if ("col" %in% names(dots)) {
    colour <- dots$col
    dots$col <- NULL
  } else {
    colour <- "red"
  }
  list(
    geom_point(aes(.data$ds, .data$y), data = object, stat = "identity",
               position = "identity", !!!dots, colour = colour)
  )
}
