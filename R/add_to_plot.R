#' Get layers to overlay significant changepoints on prophet forecast plot.
#'
#' @param model Prophet model object.
#' @param threshold Numeric, changepoints where abs(delta) >= threshold are
#'  significant. (Default 0.01)
#' @param cp_color Character, line color. (Default "red")
#' @param cp_linetype Character or integer, line type. (Default "dashed")
#' @param trend Logical, if FALSE, do not draw trend line. (Default TRUE)
#' @param ... Other arguments passed on to layers.
#'
#' @return A list of ggplot2 layers.
#'
#' @examples
#' \dontrun{
#' plot(model, fcst) + add_changepoints_to_plot(model)
#' }
#'
#' @export
add_changepoints_to_plot <- function(model, threshold = 0.01, cp_color = "red",
                                     cp_linetype = "dashed", trend = TRUE, ...) {
  dots <- rlang::dots_list(...)

  color <- cp_color
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

  linetype <- cp_linetype
  if ("linetype" %in% names(dots)) {
    linetype <- dots$linetype
    dots$linetype <- NULL
  }

  changepoints <- get_changepoints(model)
  autolayer.prophet_changepoint(
    changepoints, color = color, linetype = linetype, show_trend = trend, !!!dots)
}

#' @importFrom ggplot2 aes geom_line
#' @importFrom rlang .data !!!
#'
#' @export
add_trend_to_plot <- function(model = NULL, color = "red", ...) {
  colour <- color
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

  fore <- NULL
  if (!is.null(model)) {
    data <- model$history
    model$uncertainty.samples <- 0  # to speed up predict()
    fore <- stats::predict(model, data)
  }
  geom_line(aes(.data$ds, .data$trend), data = fore, stat = "identity",
            position = "identity", !!!dots, colour = colour)
}

#' @export
add_outliers_to_plot <- function(model, p_limit = 0.05, color = "red", ...) {
  if (!memoise::has_cache(detect_outliers)(model, p_limit)) {
    if (interactive()) message("Detecting outliers. Please wait...")
  }
  outliers <- detect_outliers(model, p_limit = p_limit)
  autolayer.prophet_outlier(outliers, color, ...)
}
