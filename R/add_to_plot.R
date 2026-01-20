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
#' @importFrom ggplot2 geom_vline
#' @importFrom rlang !!!
#'
#' @export
add_changepoints_to_plot <- function(model, threshold = 0.01, cp_color = "red",
                                     cp_linetype = "dashed", trend = TRUE, ...) {
  colour <- cp_color
  linetype <- cp_linetype
  dots <- list(...)
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
  if ("linetype" %in% names(dots)) {
    linetype <- dots$linetype
    dots$linetype <- NULL
  }

  layers <- list()
  if (trend) {
    trend_layer <- add_trend_to_plot(color = colour)
    layers <- append(layers, trend_layer)
  }
  signif_changepoints <- model$changepoints[abs(model$params$delta) >= threshold]
  cp_layer <- geom_vline(
    mapping = NULL, data = NULL, stat = "identity", position = "identity",
    !!!dots, color = colour, linetype = linetype,
    xintercept = signif_changepoints)
  layers <- append(layers, cp_layer)
  return(layers)
}

#' @importFrom ggplot2 aes geom_line
#' @importFrom rlang .data !!!
#'
#' @export
add_trend_to_plot <- function(model, color = "red", ...) {
  colour <- color
  dots <- list(...)
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
  geom_line(aes(.data$ds, .data$trend), data = NULL, stat = "identity",
            position = "identity", !!!dots, colour = colour)
}

#' @export
add_outliers_to_plot <- function(model, p_limit = 0.05, ...) {
  if (!memoise::has_cache(detect_outliers)(model, p_limit)) {
    if (interactive()) message("Detecting outliers. Please wait...")
  }
  outliers <- detect_outliers(model, p_limit = p_limit)
  autolayer.prophet_outlier(outliers, ...)
}
