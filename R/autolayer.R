#' Return ggplot layer objects to overlay prophet plot.
#'
#' @param object Object.
#' @param ... Other arguments passed on to layers.
#'
#' @return A list of layer objects.
#'
#' @examples
#' \dontrun{
#' cpts <- prophet_pick_changepoints(m)
#' plot(m, fcst) + autolayer(cpts)
#'
#' outliers <- prophet_detect_outliers(m)
#' plot(m, fcst) + autolayer(outliers)
#' }
NULL

#' @import ggplot2
#' @export
autolayer.prophet_changepoint <- function(object, ...) {
  color <- list(...)$color
  if (is.null(color)) color <- "red"
  list(
    geom_line(aes_string("ds", "trend"), color = color, ...),
    geom_vline(xintercept = object$changepoint[-1],
               color = color, linetype = "dashed", ...)
  )
}

#' @import ggplot2
#' @export
autolayer.prophet_outlier <- function(object, ...) {
  color <- list(...)$color
  if (is.null(color)) color <- "red"
  list(
    geom_point(data = object, aes_string("ds", "y"), color = color, ...)
  )
}
