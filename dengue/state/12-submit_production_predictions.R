# 12-submit_production_predictions.R

library(reticulate)
library(data.table)
library(dotenv)

# 0. API / Python setup ----------------------------------------------------

dotenv::load_dot_env(".env")

api_key <- Sys.getenv("API_KEY")

if (!nzchar(api_key)) {
  stop("API_KEY was not found in the environment.")
}

python_path <- "/home/dvilla/.virtualenvs/imdc-mosq/bin/python"

if (!file.exists(python_path)) {
  stop("Python executable not found: ", python_path)
}

reticulate::use_python(
  python_path,
  required = TRUE
)

reticulate::py_config()

mosq <- reticulate::import("mosqlient")


# 1. Submission metadata --------------------------------------------------

repository <- "dievillano/3rd_imdc_bsc_ghr"

commit <- "54002fa44dbee5a38e7c7fd97107b11d1d945fe7"

submission_path <- fs::path(
  "dengue",
  "state",
  "outputs",
  "production",
  "state_forecast_submission.csv"
)


# 2. Read submission -------------------------------------------------------

submission <- readr::read_csv(
  submission_path,
  show_col_types = FALSE
)

prediction_cols <- c(
  "date",
  "pred",
  "lower_95",
  "lower_90",
  "lower_80",
  "lower_50",
  "upper_50",
  "upper_80",
  "upper_90",
  "upper_95"
)

required_cols <- c(
  "uf_code",
  prediction_cols
)

missing_cols <- setdiff(
  required_cols,
  names(submission)
)

if (length(missing_cols) > 0L) {
  stop(
    "Missing columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

submission <- submission |>
  dplyr::mutate(
    date = as.character(as.Date(date))
  )


# 3. Pre-submission QA -----------------------------------------------------

if (nrow(submission) != 26L * 52L) {
  stop(
    "Unexpected number of rows: ",
    nrow(submission),
    ". Expected 1352."
  )
}

if (dplyr::n_distinct(submission$uf_code) != 26L) {
  stop("Expected forecasts for 26 states.")
}

bad_state_counts <- submission |>
  dplyr::count(uf_code) |>
  dplyr::filter(n != 52L)

if (nrow(bad_state_counts) > 0L) {
  stop("At least one state does not have exactly 52 forecasts.")
}

duplicate_rows <- submission |>
  dplyr::count(uf_code, date) |>
  dplyr::filter(n != 1L)

if (nrow(duplicate_rows) > 0L) {
  stop("Duplicate state/date combinations detected.")
}

numeric_prediction_cols <- setdiff(
  prediction_cols,
  "date"
)

has_bad_values <- submission |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(numeric_prediction_cols),
      ~ any(is.na(.x) | !is.finite(.x) | .x < 0)
    )
  ) |>
  unlist() |>
  any()

if (has_bad_values) {
  stop(
    "Missing, non-finite, or negative prediction values detected."
  )
}

bad_intervals <- submission |>
  dplyr::filter(
    !(
      lower_95 <= lower_90 &
        lower_90 <= lower_80 &
        lower_80 <= lower_50 &
        lower_50 <= pred &
        pred <= upper_50 &
        upper_50 <= upper_80 &
        upper_80 <= upper_90 &
        upper_90 <= upper_95
    )
  )

if (nrow(bad_intervals) > 0L) {
  stop("Prediction interval ordering violation detected.")
}

cat(
  "\nPre-submission QA passed.\n",
  "Rows: ", nrow(submission), "\n",
  "States: ", dplyr::n_distinct(submission$uf_code), "\n",
  "Forecasts per state: 52\n",
  "Date range: ",
  min(submission$date),
  " to ",
  max(submission$date),
  "\n\n",
  sep = ""
)


# 4. Upload helper ---------------------------------------------------------

upload_one_prediction <- function(data, uf_code) {
  
  prediction <- data |>
    dplyr::arrange(date) |>
    dplyr::select(
      dplyr::all_of(prediction_cols)
    )
  
  description <- paste0(
    "Final 3rd IMDC 2026 state-level dengue forecasts ",
    "from the BSC-GHR legacy model."
  )
  
  cat(
    "Uploading state ",
    uf_code,
    ", ",
    nrow(prediction),
    " weeks...\n",
    sep = ""
  )
  
  mosq$upload_prediction(
    api_key = api_key,
    disease = "A90",
    repository = repository,
    description = description,
    commit = commit,
    case_definition = "probable",
    published = TRUE,
    adm_level = 1L,
    adm_0 = "BRA",
    adm_1 = as.integer(uf_code),
    prediction = prediction
  )
}


# 5. Submit ---------------------------------------------------------------

submission |>
  dplyr::arrange(
    uf_code,
    date
  ) |>
  dplyr::group_by(
    uf_code
  ) |>
  dplyr::group_walk(
    \(data, key) {
      
      upload_one_prediction(
        data = data,
        uf_code = key$uf_code
      )
    }
  )

cat("\nDone.\n")
