#' @importFrom dplyr mutate filter left_join pull
#' @importFrom rlang .data
pick_na_dates <- function(model) {
  if ("daily" %in% names(model$seasonalities)) return(NA)

  data <- model$history |> mutate(ds = as.Date(.data$ds))
  date_range <- range(data$ds)
  dates <- seq.Date(date_range[1], date_range[2], by = "days")
  na_dates <- data.frame(ds = dates) |>
    left_join(data, by = "ds") |>
    filter(is.na(.data$y)) |>
    pull(.data$ds)
  na_dates
}
