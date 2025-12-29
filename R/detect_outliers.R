#' Detect Outliers
#'
#' @export
detect_outliers <- function(model, p_limit = 0.05) {
  data <- model$history
  model$uncertainty.samples <- 0  # to speed up predict()

  df_outliers <- data.frame()
  ind <- NULL
  pb_value <- 0
  pb_max <- p_limit
  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = pb_max, style = 3)
  }
  while (TRUE) {
    if (!is.null(ind)) {
      data <- data[-ind, ]
    }
    if (nrow(data) == 0L) {
      stop("No Data")
    }

    m <- do.call(prophet::prophet, args = model)
    exreg_names <- names(model$extra_regressors)
    for (i in seq_along(model$extra_regressors)) {
      exreg <- model$extra_regressors[[i]]
      m <- prophet::add_regressor(m, exreg_names[i], exreg$prior.scale, exreg$standardize, exreg$mode)
    }
    m <- suppressMessages(prophet::fit.prophet(m, data))
    fore <- stats::predict(m, data)
    residuals <- data$y - fore$yhat

    result_test <- outliers::grubbs.test(residuals, type = 10)
    if (result_test$p.value < p_limit) {
      if(startsWith(result_test$alternative, "lowest")) {
        ind <- which.min(residuals)
      } else {
        ind <- which.max(residuals)
      }
      row <- data[ind, c("ds", "y"), drop = FALSE]
      row$resid <- residuals[ind]
      row$p_value <- result_test$p.value
      pb_value <- max(pb_value, row$p_value)
      if (interactive()) utils::setTxtProgressBar(pb, pb_value)
      df_outliers <- rbind(df_outliers, row)
    } else {
      if (interactive()) utils::setTxtProgressBar(pb, pb_max)
      break
    }
  }
  if (interactive()) close(pb)

  if (!"daily" %in% names(model$seasonalities)) {
    df_outliers$ds <- as.Date(df_outliers$ds)
  }

  structure(df_outliers,
            class = c("prophet_outlier", "tbl_df", "tbl", "data.frame"),
            na_dates = pick_na_dates(model))
}

#' #' @export
#' detect_outliers_one <- function(model, prob_limit = 0.05, data = NULL) {
#'   if (is.null(data)) {
#'     data <- model$history
#'   }
#'
#'   model$interval.width <- 1 - prob_limit
#'   fore <- predict(model, data)
#'
#'   ind <- data$y < fore$yhat_lower | fore$yhat_upper < data$y
#'   data_outliers <- data[ind, , drop = FALSE]
#'   fore_outliers <- fore[ind, , drop = FALSE]
#'   residuals <- data_outliers$y - fore_outliers$yhat
#'
#'   df_outliers <- data.frame(
#'     ds = data_outliers$ds,
#'     y = data_outliers$y,
#'     resid = residuals
#'   )
#'
#'   structure(df_outliers, class = c("prophet_outlier", "tbl_df", "tbl", "data.frame"))
#' }

# detect_outliers2 <- function(model, prob_limit = 0.05) {
#   data <- model$history
#
#   m <- do.call(prophet, args = model)
#   m <- suppressMessages(fit.prophet(m, data))
#   df_outliers <- detect_outliers_one(m, prob_limit = prob_limit, data = data)
#   fore <- predict(m, data)
#   g <- plot(m, fore) + autolayer(df_outliers)
#   print(g)
#
#   while (TRUE) {
#     df_outliers_last <- df_outliers
#     data_without_outliers <- data |> filter(!ds %in% df_outliers$ds)
#     m <- do.call(prophet, args = model)
#     m$changepoints <- NULL
#     m <- suppressMessages(fit.prophet(m, data_without_outliers))
#
#     set.seed(314)
#     df_outliers <- detect_outliers_one(m, prob_limit = prob_limit, data = data)
#
#     fore <- predict(m, data)
#     g <- plot(m, fore) + autolayer(df_outliers)
#     print(g)
#
#     diff <- setdiff(df_outliers_last$ds, df_outliers$ds)
#
#     print(length(diff))
#     if (length(diff) == 0) break
#   }
#
#   structure(df_outliers, class = c("prophet_outlier", "tbl_df", "tbl", "data.frame"))
# }
