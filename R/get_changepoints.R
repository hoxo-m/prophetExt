#' @export
get_changepoints <- function(model, threshold = 0.01) {
  cp_index <- c(1, which(abs(model$params$delta) >= threshold) + 1)
  changepoints <- c(model$start, model$changepoints)[cp_index]
  growth_rate <- model$params$k + c(0, cumsum(model$params$delta))[cp_index]
  delta <- c(NA_real_, model$params$delta)[cp_index]
  df <- data.frame(ds = changepoints,
                   growth_rate = growth_rate,
                   delta = delta)

  if (!"daily" %in% names(model$seasonalities)) {
    df$ds <- as.Date(df$ds)
  }

  structure(df, class = c("prophet_changepoint", "tbl_df", "tbl", "data.frame"))
}
