#' Calendar plot
#'
#' @param object Object.
#' @param ... Other arguments.
#'
#' @export
plot_residuals_calendar <- function(
    object, begin = NULL, end = NULL, drop_empty = TRUE, show_na,
    colors = c(low = "red", mid = "white", high = "blue", na = "lightgrey"),
    week_start = 7, guide = ggplot2::guide_colorbar("Residual")) {
  UseMethod("plot_residuals_calendar")
}

#' Calendar plot for residuals of prophet model
#'
#' @param object prophet model.
#' @param begin date or character value.
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate select left_join
#'
#' @export
plot_residuals_calendar.prophet <- function(
    object, begin = NULL, end = NULL, drop_empty = TRUE, show_na = FALSE,
    colors = c(low = "red", mid = "white", high = "blue", na = "lightgrey"),
    week_start = 7, guide = ggplot2::guide_colorbar("Residual")) {

  model <- object
  data <- model$history
  model$uncertainty.samples <- 0L  # to speed up predict()
  fore <- stats::predict(model, data)
  df_resid <- fore |>
    left_join(data, by = "ds") |>
    mutate(ds = as.Date(.data$ds), resid = .data$y - .data$yhat) |>
    select(ds = .data$ds, y = .data$y, resid = .data$resid)

  if (show_na) {
    na_dates <- pick_na_dates(model)
  } else {
    na_dates <- NULL
  }

  plot_calendar(df_resid, begin = begin, end = end, drop_empty = drop_empty,
                colors = colors, week_start = week_start, guide = guide,
                na_dates = na_dates)
}

#' Calendar plot for outliers
#'
#' @param object prophet_outlier object.
#' @param begin date or character value.
#'
#' @export
plot_residuals_calendar.prophet_outlier <- function(
    object, begin = NULL, end = NULL, drop_empty = TRUE, show_na = TRUE,
    colors = c(low = "red", mid = "white", high = "blue", na = "lightgrey"),
    week_start = 7, guide = ggplot2::guide_colorbar("Residual")) {

  if (show_na) {
    na_dates <- attr(object, "na_dates")
  } else {
    na_dates <- NULL
  }

  plot_calendar(object, begin = begin, end = end, drop_empty = drop_empty,
                colors = colors, week_start = week_start, guide = guide,
                na_dates = na_dates)
}

#' @importFrom lubridate year month day wday
#' @importFrom dplyr across filter mutate group_by ungroup left_join if_else
#' @importFrom ggplot2 ggplot aes geom_tile geom_text facet_grid
#'             scale_x_continuous scale_y_continuous scale_fill_gradient2
#'             scale_alpha xlab ylab theme_get theme_gray theme_bw
#' @importFrom rlang .data
plot_calendar <- function(df_resid, begin, end, drop_empty, colors, week_start,
                          guide, na_dates) {

  colors <- get_colors(colors)
  drop_empty <- get_drop_empty(drop_empty)
  if (!is.null(begin)) {
    begin <- as.Date(begin)
    df_resid <- df_resid |> filter(.data$ds >= begin)
  }
  if (!is.null(end)) {
    end <- as.Date(end)
    df_resid <- df_resid |> filter(.data$ds <= end)
  }

  year_range <- range(year(df_resid$ds))
  dates <- seq(as.Date(sprintf("%d-01-01", year_range[1])),
               as.Date(sprintf("%d-12-31", year_range[2])), by = "days")

  my_wday <- function(x) lubridate::wday(x,label = TRUE, week_start = week_start)
  col_names <- c("year", "month", "day", "wday")
  df <- data.frame(ds = dates) |>
    left_join(df_resid, by = "ds") |>
    mutate(across(.data$ds, list(year, month, day, my_wday), .names = "{col_names}")) |>
    mutate(wday = factor(.data$wday, levels = rev(levels(.data$wday)))) |>
    group_by(.data$year, .data$month) |>
    mutate(week = cumsum(as.integer(.data$wday) == 7)) |>
    ungroup() |>
    mutate(resid = if_else(is.na(.data$resid), 0, .data$resid))

  if (drop_empty$year) {
    df <- df |>
      group_by(.data$year) |>
      filter(any(.data$resid != 0, na.rm = TRUE)) |>
      ungroup()
  }
  if (drop_empty$month) {
    df <- df |>
      group_by(.data$month) |>
      filter(any(.data$resid != 0, na.rm = TRUE)) |>
      ungroup()
  }
  if (!is.null(na_dates)) {
    df <- df |>
      mutate(resid = if_else(.data$ds %in% na_dates, NA_real_, .data$resid))
  }

  df <- df |>
    mutate(label = if_else(.data$resid == 0, "", as.character(.data$day))) |>
    mutate(alpha = if_else(.data$resid == 0, 0, 1))

  if (identical(theme_get(), theme_gray())) {
    ggtheme <- theme_bw
  } else {
    ggtheme <- theme_get
  }

  ggplot(df, aes(.data$week, .data$wday)) +
    geom_tile(aes(fill = .data$resid, alpha = .data$alpha), na.rm = TRUE) +
    geom_text(aes(label = .data$label), na.rm = TRUE) +
    facet_grid(.data$year ~ .data$month) +
    scale_x_continuous(breaks = NULL) +
    scale_fill_gradient2(low = colors$low, mid = colors$mid, high = colors$high,
                         na.value = colors$na, guide = guide) +
    scale_alpha(guide = NULL) +
    xlab(NULL) + ylab(NULL) +
    ggtheme()
}

get_or_default <- function(x, default) {
  if (is.null(x)) {
    default
  } else {
    x
  }
}

get_colors <- function(colors) {
  colors <- as.list(colors)
  list(
    low = get_or_default(colors$low, "red"),
    mid = get_or_default(colors$mid, "white"),
    high = get_or_default(colors$high, "blue"),
    na = get_or_default(colors$na, "lightgrey")
  )
}

get_drop_empty <- function(drop_empty) {
  if (is.null(names(drop_empty)) && length(drop_empty) == 1L) {
    drop_empty <- unlist(drop_empty)
    list(year = drop_empty, month = drop_empty)
  } else {
    drop_empty <- as.list(drop_empty)
    if (is.null(names(drop_empty))) {
      warning("invalid drop_empty")
    }
    list(
      year = get_or_default(drop_empty$year, TRUE),
      month = get_or_default(drop_empty$month, TRUE)
    )
  }
}
