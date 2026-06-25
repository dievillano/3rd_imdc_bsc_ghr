plot_acf <- function(
    data,
    value,
    group = NULL,
    time = NULL,
    max_lag = 24,
    ncol = NULL
) {
  value_quo <- rlang::enquo(value)
  group_quo <- rlang::enquo(group)
  time_quo  <- rlang::enquo(time)
  
  has_group <- !rlang::quo_is_null(group_quo)
  has_time  <- !rlang::quo_is_null(time_quo)
  
  if (has_time) {
    data <- if (has_group) {
      data |> dplyr::arrange(!!group_quo, !!time_quo)
    } else {
      data |> dplyr::arrange(!!time_quo)
    }
  }
  
  if (has_group) {
    acf_df <- data |>
      dplyr::group_by(!!group_quo) |>
      dplyr::group_modify(
        ~ {
          x <- dplyr::pull(.x, !!value_quo)
          
          acf_obj <- forecast::Acf(
            x,
            lag.max = max_lag,
            plot = FALSE,
            demean = TRUE,
            na.action = na.contiguous
          )
          
          tibble::tibble(
            lag = as.numeric(acf_obj$lag),
            acf = as.numeric(acf_obj$acf)
          )
        }
      ) |>
      dplyr::ungroup()
    
    ggplot2::ggplot(acf_df, ggplot2::aes(x = lag, y = acf)) +
      ggplot2::geom_col(width = 0.05) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", rlang::as_name(group_quo))),
        ncol = ncol
      ) +
      ggplot2::theme_bw()
    
  } else {
    acf_obj <- forecast::Acf(
      dplyr::pull(data, !!value_quo),
      lag.max = max_lag,
      plot = FALSE,
      demean = TRUE,
      na.action = na.contiguous
    )
    
    acf_df <- tibble::tibble(
      lag = as.numeric(acf_obj$lag),
      acf = as.numeric(acf_obj$acf)
    )
    
    ggplot2::ggplot(acf_df, ggplot2::aes(x = lag, y = acf)) +
      ggplot2::geom_col(width = 0.05) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::theme_bw()
  }
}