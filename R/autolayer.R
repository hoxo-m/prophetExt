#' @importFrom ggplot2 autolayer
#' @export
ggplot2::autolayer

#' @importFrom ggplot2 geom_vline
#' @importFrom rlang !!!
#'
#' @export
autolayer.prophet_changepoint <- function(object, ...) {

  changepoints <- object
  if (nrow(changepoints) <= 1L) {
    warning("no changepoints")
    return(NULL)
  }

  dots <- rlang::dots_list(...)

  color <- "red"
  if ("col" %in% names(dots)) {
    color <- dots$col
    dots$col <- NULL
  }
  if ("color" %in% names(dots)) {
    color <- dots$color
    dots$color <- NULL
  }
  if ("colour" %in% names(dots)) {
    color <- dots$colour
    dots$colour <- NULL
  }

  linetype <- "dashed"
  if ("linetype" %in% names(dots)) {
    linetype <- dots$linetype
    dots$linetype <- NULL
  }

  show_trend <- TRUE
  if (all(c("show_trend", "trend") %in% names(dots))) {
    if (dots$show_trend != dots$trend) {
      stop("conflicted show_trend and trend")
    }
  }
  if ("show_trend" %in% names(dots)) {
    show_trend <- dots$show_trend
    dots$show_trend <- NULL
  }
  if ("trend" %in% names(dots)) {
    show_trend <- dots$trend
    dots$trend <- NULL
  }

  trend_layer <- list()
  if (show_trend) {
    trend_layer <- add_trend_to_plot(model = NULL, color = color, !!!dots)
  }

  cp_layer <- geom_vline(
    mapping = NULL, data = NULL, stat = "identity", position = "identity",
    !!!dots, colour = color, linetype = linetype,
    xintercept = changepoints$ds[-1])

  c(trend_layer, cp_layer)
}

#' @importFrom ggplot2 autolayer aes geom_point
#' @importFrom rlang .data !!!
#'
#' @export
autolayer.prophet_outlier <- function(object, ...) {

  outliers <- object
  if (nrow(outliers) == 0L) {
    warning("no outliers")
    return(NULL)
  }

  dots <- rlang::dots_list(...)

  color <- "red"
  if ("col" %in% names(dots)) {
    color <- dots$col
    dots$col <- NULL
  }
  if ("color" %in% names(dots)) {
    color <- dots$color
    dots$color <- NULL
  }
  if ("colour" %in% names(dots)) {
    color <- dots$colour
    dots$colour <- NULL
  }

  geom_point(aes(.data$ds, .data$y), data = outliers, stat = "identity",
             position = "identity", !!!dots, colour = color)
}
