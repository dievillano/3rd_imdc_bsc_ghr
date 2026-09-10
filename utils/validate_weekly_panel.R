validate_weekly_panel <- function(
    data,
    unit,
    date,
    epiweek = NULL,
    expected_step = "week",
    spatial_units = NULL,
    show_missing = TRUE,
    fail = c("error", "warning")
) {
  
  unit_char <- rlang::as_name(rlang::ensym(unit))
  date_char <- rlang::as_name(rlang::ensym(date))
  epiweek_quo <- rlang::enquo(epiweek)
  has_epiweek <- !rlang::quo_is_null(epiweek_quo)
  
  fail <- match.arg(fail)
  
  validation_fail <- function(msg) {
    if (fail == "error") {
      cli::cli_abort(msg)
    } else {
      cli::cli_alert_danger(msg[-1])
    }
  }
  
  cli::cli_h1("Validating weekly panel dataset")
  
  n_units <- data |>
    dplyr::pull({{ unit }}) |>
    dplyr::n_distinct()
  
  n_dates <- data |>
    dplyr::pull({{ date }}) |>
    dplyr::n_distinct()
  
  min_date <- min(dplyr::pull(data, {{ date }}), na.rm = TRUE)
  max_date <- max(dplyr::pull(data, {{ date }}), na.rm = TRUE)
  
  cli::cli_alert_info("{unit_char} units: {cli::col_blue(n_units)}")
  cli::cli_alert_info("Time points: {cli::col_blue(n_dates)}")
  cli::cli_alert_info(
    paste0(
      "Date range: {cli::col_blue(format(min_date, '%d %B %Y'))} ", 
      "to {cli::col_blue(format(max_date, '%d %B %Y'))}")
  )
  
  if (has_epiweek) {
    min_epiweek <- min(dplyr::pull(data, !!epiweek_quo), na.rm = TRUE)
    max_epiweek <- max(dplyr::pull(data, !!epiweek_quo), na.rm = TRUE)
    
    cli::cli_alert_info(
      "Epiweek range: {cli::col_blue(min_epiweek)} to {cli::col_blue(max_epiweek)}"
    )
  }
  
  duplicates <- data |>
    dplyr::count({{ unit }}, {{ date }}) |>
    dplyr::filter(n > 1)
  
  if (nrow(duplicates) > 0) {
    validation_fail(c(
      "!" = "Duplicated unit-date combinations detected.",
      "x" = "{nrow(duplicates)} duplicates found."
    ))
  } else {
    cli::cli_alert_success("No duplicated unit-date combinations.")
  }
  
  counts <- data |>
    dplyr::count({{ unit }}, name = "n_dates")
  
  if (dplyr::n_distinct(counts$n_dates) != 1) {
    validation_fail(c(
      "!" = "Unequal number of time points across units.",
      "x" = "Min: {min(counts$n_dates)} ",
      "x" = "Max: {max(counts$n_dates)}"
    ))
  } else {
    cli::cli_alert_success("All units have equal time length.")
  }
  
  gaps <- data |>
    dplyr::arrange({{ unit }}, {{ date }}) |>
    dplyr::group_by({{ unit }}) |>
    dplyr::mutate(
      time_diff = as.numeric({{ date }} - dplyr::lag({{ date }}))
    ) |>
    dplyr::filter(!is.na(time_diff) & time_diff != 7) |>
    dplyr::ungroup()
  
  if (nrow(gaps) > 0) {
    validation_fail(c(
      "!" = "Non-consecutive weekly time series detected.",
      "x" = "{nrow(gaps)} gaps found."
    ))
  } else {
    cli::cli_alert_success("Time series are fully consecutive weekly sequences.")
  }
  
  if (has_epiweek) {
    epiweek_check <- data |>
      dplyr::distinct({{ date }}, !!epiweek_quo) |>
      dplyr::count({{ date }}, name = "n_epiweeks") |>
      dplyr::filter(n_epiweeks > 1)
    
    if (nrow(epiweek_check) > 0) {
      validation_fail(c(
        "!" = "One or more dates map to multiple epiweeks.",
        "x" = "{nrow(epiweek_check)} problematic dates found."
      ))
    } else {
      cli::cli_alert_success("Each date maps to a unique epiweek.")
    }
  }
  
  if (show_missing) {
    cli::cli_h2("Missingness summary")
    
    missing_summary <- data |>
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(),
          \(x) sum(is.na(x))
        )
      ) |>
      tidyr::pivot_longer(
        dplyr::everything(),
        names_to = "variable",
        values_to = "n_missing"
      ) |>
      dplyr::mutate(
        pct_missing = n_missing / nrow(data)
      ) |>
      dplyr::arrange(dplyr::desc(n_missing))
    
    print(missing_summary)
    
    if (any(missing_summary$n_missing > 0)) {
      cli::cli_alert_warning("Missing values detected.")
    } else {
      cli::cli_alert_success("No missing values detected.")
    }
  }
  
  if (!is.null(spatial_units)) {
    
    data_units <- data |>
      dplyr::distinct({{ unit }}) |>
      dplyr::pull({{ unit }}) |>
      as.character()
    
    spatial_units <- as.character(spatial_units)
    
    missing_in_spatial <- setdiff(data_units, spatial_units)
    missing_in_data <- setdiff(spatial_units, data_units)
    
    if (length(missing_in_spatial) > 0 || length(missing_in_data) > 0) {
      msg <- c("!" = "Mismatch between data and spatial units.")
      
      if (length(missing_in_spatial) > 0) {
        msg <- c(
          msg,
          "x" = paste(
            "Present in data but missing in spatial definitions:",
            paste(missing_in_spatial, collapse = ", ")
          )
        )
      }
      
      if (length(missing_in_data) > 0) {
        msg <- c(
          msg,
          "x" = paste(
            "Present in spatial definitions but missing in data:",
            paste(missing_in_data, collapse = ", ")
          )
        )
      }
      
      validation_fail(msg)
    } else {
      cli::cli_alert_success("Spatial units match spatial definitions.")
    }
  }
  
  cli::cli_alert_success("Weekly panel validation completed successfully!")
  
  invisible(TRUE)
}