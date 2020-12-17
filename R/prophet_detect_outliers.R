#' Detect outliers using Grubbs test.
#'
#' @param model Prophet model object.
#' @param p_limit Numeric, limit of p-value for Grubbs test. Default 0.05.
#' @param recursive Logical, whether to search recursively. Default TRUE.
#'
#' @return A data frame consists of ds, y, residuals and p values.
#'
#' @examples
#' \dontrun{
#' m <- prophet(df)
#' prophet_detect_outliers(m)
#' }
#'
#' @import prophet
#' @importFrom outliers grubbs.test
#' @importFrom stats predict
#'
#' @export
prophet_detect_outliers <- function(model, p_limit = 0.05, recursive = TRUE) {
  data_hist <- model$history
  df <- data.frame(ds = Sys.Date()[-1], y = double(), resid = double(), p_values = double())
  while(TRUE) {
    data_hist <- data_hist[!(data_hist$ds %in% df$ds), ]
    m <- do.call(prophet, args = model)

    exreg_names <- names(model$extra_regressors)
    for (i in seq_along(model$extra_regressors)) {
      exreg <- model$extra_regressors[[i]]
      m <- add_regressor(m, exreg_names[i], exreg$prior.scale, exreg$standardize, exreg$mode)
    }
    m <- fit.prophet(m, data_hist)

    resid_df <- predict(m, data_hist)
    resid_df <- merge(resid_df, data_hist, by = "ds")[, c("ds", "y", "yhat")]
    resid_df$resid <- resid_df$y - resid_df$yhat

    n_outlier <- nrow(df)
    while(TRUE) {
      result_test <- grubbs.test(resid_df$resid, type = 10)
      if (result_test$p.value < p_limit) {
        if(startsWith(result_test$alternative, "lowest")) {
          ind <- which.min(resid_df$resid)
        } else {
          ind <- which.max(resid_df$resid)
        }
        row <- resid_df[ind, , drop=FALSE]
        row$p_value <- result_test$p.value
        df <- rbind(df, row)
      } else {
        break
      }
      resid_df <- resid_df[-ind, , drop=FALSE]
    }
    if (n_outlier == nrow(df) || !recursive) {
      break
    } else {
      message(sprintf("Detect %d outliers", nrow(df) - n_outlier))
    }
  }
  df <- df[order(df$ds), , drop=FALSE]
  rownames(df) <- seq_len(nrow(df))
  class(df) <- c("prophet_outlier", class(df))
  df
}
