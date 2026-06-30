sprint2026_path <- here::here()

dengue_path <- fs::path(sprint2026_path, "dengue/state")
outputs_path <- fs::path(dengue_path, "outputs")

predictions_path <- fs::path(outputs_path, "val_predictions")
val_submission_path  <- fs::path(outputs_path, "val_submission")

fs::dir_create(val_submission_path)

resources_path <- fs::path("resources/health_region")

utils_path <- fs::path(sprint2026_path, "utils")
utils_filepaths <- fs::dir_ls(utils_path)
purrr::walk(utils_filepaths, source)

# 1. Read predictions

prediction_files <- fs::dir_ls(
  predictions_path,
  glob = "*.rds"
) |>
  stringr::str_subset("split_\\d+_legacy_model\\.rds$")

make_state_submission <- function(file) {
  
  preds <- readRDS(file)
  
  split_id <- stringr::str_extract(fs::path_file(file), "(?<=split_)\\d+")
  
  state_preds <- preds |>
    dplyr::mutate(
      date = as.Date(date)
    ) |>
    dplyr::group_by(uf_code, uf, epiweek, date) |>
    dplyr::summarise(
      .pred_samples = list(purrr::reduce(.pred_samples, `+`)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      q = purrr::map(
        .pred_samples,
        \(x) stats::quantile(
          x,
          probs = c(
            0.025, 0.05, 0.10, 0.25,
            0.50,
            0.75, 0.90, 0.95, 0.975
          ),
          na.rm = TRUE,
          names = FALSE
        )
      )
    ) |>
    tidyr::unnest_wider(
      q,
      names_sep = "_"
    ) |>
    dplyr::rename(
      lower_95 = q_1,
      lower_90 = q_2,
      lower_80 = q_3,
      lower_50 = q_4,
      pred     = q_5,
      upper_50 = q_6,
      upper_80 = q_7,
      upper_90 = q_8,
      upper_95 = q_9
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(lower_95, lower_90, lower_80, lower_50,
          pred, upper_50, upper_80, upper_90, upper_95),
        \(x) pmax(x, 0)
      ),
      .split_id = split_id
    ) |>
    dplyr::select(
      .split_id,
      uf_code, uf, epiweek, date,
      pred,
      lower_50, upper_50,
      lower_80, upper_80,
      lower_90, upper_90,
      lower_95, upper_95
    )
  
  state_preds
}

submission <- prediction_files |>
  purrr::map(make_state_submission) |>
  dplyr::bind_rows() |>
  dplyr::arrange(.split_id, uf_code, epiweek, date)

# Basic validation checks -------------------------------------------------

check_submission <- function(x) {
  
  stopifnot(all(lubridate::wday(x$date, week_start = 7) == 1))
  
  stopifnot(all(
    x$lower_95 <= x$lower_90,
    x$lower_90 <= x$lower_80,
    x$lower_80 <= x$lower_50,
    x$lower_50 <= x$pred,
    x$pred <= x$upper_50,
    x$upper_50 <= x$upper_80,
    x$upper_80 <= x$upper_90,
    x$upper_90 <= x$upper_95
  ))
  
  stopifnot(all(
    x$pred >= 0,
    x$lower_50 >= 0,
    x$lower_80 >= 0,
    x$lower_90 >= 0,
    x$lower_95 >= 0
  ))
  
  invisible(TRUE)
}

check_submission(submission)

geocode_lookup_table  <- readRDS(
  fs::path(resources_path, "geocode_lookup_table.rds")
)

submission |>
  dplyr::group_split(.split_id) |>
  purrr::walk(
    \(df) {
      split_id <- unique(df$.split_id)

      validate_weekly_panel(
        data = df,
        unit = uf_code,
        date = date,
        epiweek = epiweek,
        spatial_units = unique(geocode_lookup_table$uf_code),
        fail = "error"
      )

    }
  )

# Save one file with all validation splits --------------------------------

submission |> 
  dplyr::select(-epiweek) |> 
  readr::write_csv(
    fs::path(
      val_submission_path, 
      "state_validation_submission.csv"
    )
  )



