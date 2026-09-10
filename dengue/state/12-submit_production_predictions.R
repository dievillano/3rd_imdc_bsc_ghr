# 12-submit_production_predictions.R


# =============================================================================
# 0. API / Python setup
# =============================================================================

library(reticulate)
library(dotenv)

dotenv::load_dot_env(".env")

api_key <- Sys.getenv("API_KEY")

if (!nzchar(api_key)) {
  stop("API_KEY was not found in the environment.")
}

python_path <- "/home/dvilla/.virtualenvs/imdc-mosq/bin/python"

if (!file.exists(python_path)) {
  stop(
    "Python executable not found: ",
    python_path
  )
}

reticulate::use_python(
  python_path,
  required = TRUE
)

reticulate::py_config()

mosq <- reticulate::import(
  "mosqlient"
)


# =============================================================================
# 1. Submission metadata
# =============================================================================

repository <- "dievillano/3rd_imdc_bsc_ghr"

model_name <- "3rd_imdc_bsc_ghr"

model_owner <- "dievillano"

commit <- "b3af423fbf2f714612e6b8d4061daafa5bfda83c"

submission_path <- fs::path(
  "dengue",
  "state",
  "outputs",
  "production",
  "state_forecast_submission.csv"
)

upload_log_path <- fs::path(
  "dengue",
  "state",
  "outputs",
  "production",
  "mosqlimate_production_predictions.csv"
)

expected_start <- as.Date(
  "2026-10-11"
)

expected_end <- as.Date(
  "2027-10-03"
)

expected_dates <- seq.Date(
  from = expected_start,
  to = expected_end,
  by = "week"
)

expected_uf_codes <- c(
  11L,
  12L,
  13L,
  14L,
  15L,
  16L,
  17L,
  21L,
  22L,
  23L,
  24L,
  25L,
  26L,
  27L,
  28L,
  29L,
  31L,
  33L,
  35L,
  41L,
  42L,
  43L,
  50L,
  51L,
  52L,
  53L
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


# =============================================================================
# 2. Check commit
# =============================================================================

if (
  length(commit) != 1L ||
  !grepl(
    "^[0-9a-f]{40}$",
    commit
  )
) {
  stop(
    "`commit` must be a full 40-character Git commit hash."
  )
}

current_head <- system2(
  "git",
  c(
    "rev-parse",
    "HEAD"
  ),
  stdout = TRUE
)

if (
  length(current_head) != 1L ||
  current_head != commit
) {
  stop(
    "Submission commit does not match the current Git HEAD.\n",
    "Submission commit: ",
    commit,
    "\nCurrent HEAD: ",
    paste(
      current_head,
      collapse = ""
    )
  )
}

cat(
  "\nGit commit check passed.\n",
  "Commit: ",
  commit,
  "\n\n",
  sep = ""
)


# =============================================================================
# 3. Read submission
# =============================================================================

if (!file.exists(submission_path)) {
  stop(
    "Submission file not found: ",
    submission_path
  )
}

submission <- readr::read_csv(
  submission_path,
  show_col_types = FALSE
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
    paste(
      missing_cols,
      collapse = ", "
    )
  )
}

submission <- submission |>
  dplyr::mutate(
    uf_code = as.integer(
      uf_code
    ),
    date = as.Date(
      date
    )
  )


# =============================================================================
# 4. Pre-submission QA
# =============================================================================

# Exact number of state-week forecasts.

if (
  nrow(
    submission
  ) != 26L * 52L
) {
  stop(
    "Unexpected number of rows: ",
    nrow(
      submission
    ),
    ". Expected 1352."
  )
}


# Exact state set.

actual_uf_codes <- sort(
  unique(
    submission$uf_code
  )
)

if (
  !setequal(
    actual_uf_codes,
    expected_uf_codes
  )
) {
  stop(
    paste0(
      "Submitted UF codes do not match the expected 26-state set.\n",
      "Expected: ",
      paste(
        expected_uf_codes,
        collapse = ", "
      ),
      "\nFound: ",
      paste(
        actual_uf_codes,
        collapse = ", "
      )
    )
  )
}


# Exactly 52 forecasts for each state.

bad_state_counts <- submission |>
  dplyr::count(
    uf_code,
    name = "n_weeks"
  ) |>
  dplyr::filter(
    n_weeks != 52L
  )

if (
  nrow(
    bad_state_counts
  ) > 0L
) {
  print(
    bad_state_counts
  )
  
  stop(
    "At least one state does not have exactly 52 forecasts."
  )
}


# No duplicated state-date combinations.

duplicate_rows <- submission |>
  dplyr::count(
    uf_code,
    date,
    name = "n"
  ) |>
  dplyr::filter(
    n != 1L
  )

if (
  nrow(
    duplicate_rows
  ) > 0L
) {
  print(
    duplicate_rows
  )
  
  stop(
    "Duplicate state/date combinations detected."
  )
}


# Exact forecast-date sequence.

actual_dates <- sort(
  unique(
    submission$date
  )
)

if (
  !identical(
    actual_dates,
    expected_dates
  )
) {
  stop(
    paste0(
      "Forecast dates do not match the expected weekly sequence.\n",
      "Expected: ",
      expected_start,
      " to ",
      expected_end,
      "\nFound: ",
      min(
        actual_dates
      ),
      " to ",
      max(
        actual_dates
      )
    )
  )
}


# All dates should be Sundays.

if (
  !all(
    lubridate::wday(
      submission$date,
      week_start = 7
    ) == 1L
  )
) {
  stop(
    "Not all forecast dates are Sundays."
  )
}


# Prediction values must be finite and non-negative.

numeric_prediction_cols <- setdiff(
  prediction_cols,
  "date"
)

has_bad_values <- submission |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(
        numeric_prediction_cols
      ),
      ~ any(
        is.na(
          .x
        ) |
          !is.finite(
            .x
          ) |
          .x < 0
      )
    )
  ) |>
  unlist() |>
  any()

if (
  has_bad_values
) {
  stop(
    "Missing, non-finite, or negative prediction values detected."
  )
}


# Quantiles must be monotonically ordered.

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

if (
  nrow(
    bad_intervals
  ) > 0L
) {
  print(
    bad_intervals
  )
  
  stop(
    "Prediction interval ordering violation detected."
  )
}


cat(
  "\n============================================================\n",
  "PRE-SUBMISSION QA PASSED\n",
  "============================================================\n",
  "Rows: ",
  nrow(
    submission
  ),
  "\n",
  "States: ",
  dplyr::n_distinct(
    submission$uf_code
  ),
  "\n",
  "Forecasts per state: 52\n",
  "Date range: ",
  as.character(
    min(
      submission$date
    )
  ),
  " to ",
  as.character(
    max(
      submission$date
    )
  ),
  "\n",
  "Commit: ",
  commit,
  "\n\n",
  sep = ""
)


# Convert date to character for Mosqlimate.

submission <- submission |>
  dplyr::mutate(
    date = as.character(
      date
    )
  )


# =============================================================================
# 5. Check Mosqlimate registry before upload
# =============================================================================
#
# There should currently be no production predictions for this forecast
# horizon. This protects against accidentally creating duplicates if this
# script is rerun after a partial or completed upload.
# =============================================================================

existing <- mosq$get_predictions(
  api_key = api_key,
  model_name = model_name,
  model_owner = model_owner,
  adm_level = 1L,
  disease = "A90",
  imdc_year = 2026L
)

existing_index <- purrr::map_dfr(
  existing,
  \(p) {
    tibble::tibble(
      id = as.integer(
        p$id
      ),
      adm_1 = as.integer(
        p$adm_1
      ),
      start = as.character(
        p$start
      ),
      end = as.character(
        p$end
      ),
      commit = as.character(
        p$commit
      )
    )
  }
)

existing_production <- existing_index |>
  dplyr::filter(
    start == as.character(
      expected_start
    ),
    end == as.character(
      expected_end
    )
  )

if (
  nrow(
    existing_production
  ) > 0L
) {
  print(
    existing_production,
    n = Inf
  )
  
  stop(
    paste0(
      "Production predictions already exist for the ",
      "2026-2027 forecast horizon. ",
      "No new predictions were uploaded."
    )
  )
}

cat(
  "Mosqlimate registry pre-check passed.\n",
  "No existing predictions found for ",
  expected_start,
  " to ",
  expected_end,
  ".\n\n",
  sep = ""
)


# =============================================================================
# 6. Upload helper
# =============================================================================

upload_one_prediction <- function(
    data,
    uf_code
) {
  
  prediction <- data |>
    dplyr::arrange(
      date
    ) |>
    dplyr::select(
      dplyr::all_of(
        prediction_cols
      )
    )
  
  if (
    nrow(
      prediction
    ) != 52L
  ) {
    stop(
      "State ",
      uf_code,
      " does not contain exactly 52 forecast rows."
    )
  }
  
  description <- paste0(
    "Final 3rd IMDC 2026 state-level dengue forecasts ",
    "from the BSC-GHR legacy model, with corrected ",
    "population exposure handling."
  )
  
  cat(
    "Uploading state ",
    uf_code,
    ", ",
    nrow(
      prediction
    ),
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
    adm_1 = as.integer(
      uf_code
    ),
    prediction = prediction
  )
}


# =============================================================================
# 7. Submit
# =============================================================================
#
# IDs are written to disk after every successful upload. If an upload fails
# partway through, do NOT simply rerun this script. Inspect the saved log and
# the Mosqlimate registry first.
# =============================================================================

submission_by_state <- submission |>
  dplyr::arrange(
    uf_code,
    date
  ) |>
  (\(x) split(
    x,
    x$uf_code
  ))()


uploaded_predictions <- tibble::tibble(
  uf_code = integer(),
  prediction_id = integer(),
  commit = character()
)


for (
  uf_code in names(
    submission_by_state
  )
) {
  
  state_data <- submission_by_state[[uf_code]]
  
  result <- tryCatch(
    {
      upload_one_prediction(
        data = state_data,
        uf_code = uf_code
      )
    },
    error = function(e) {
      
      cat(
        "\n============================================================\n",
        "UPLOAD FAILED\n",
        "============================================================\n",
        "State: ",
        uf_code,
        "\n",
        "Error: ",
        conditionMessage(
          e
        ),
        "\n\n",
        sep = ""
      )
      
      stop(
        paste0(
          "Submission stopped after an upload failure. ",
          "Do not rerun blindly. Check the Mosqlimate registry ",
          "and the local upload log first."
        ),
        call. = FALSE
      )
    }
  )
  
  new_row <- tibble::tibble(
    uf_code = as.integer(
      uf_code
    ),
    prediction_id = as.integer(
      result$id
    ),
    commit = commit
  )
  
  uploaded_predictions <- dplyr::bind_rows(
    uploaded_predictions,
    new_row
  )
  
  readr::write_csv(
    uploaded_predictions,
    upload_log_path
  )
  
  cat(
    "  -> Prediction ID: ",
    new_row$prediction_id,
    "\n",
    sep = ""
  )
}


# =============================================================================
# 8. Local upload-log QA
# =============================================================================

if (
  nrow(
    uploaded_predictions
  ) != 26L
) {
  stop(
    "Upload finished with an unexpected number of recorded predictions."
  )
}

if (
  !setequal(
    uploaded_predictions$uf_code,
    expected_uf_codes
  )
) {
  stop(
    "Uploaded UF codes do not match the expected state set."
  )
}

if (
  anyDuplicated(
    uploaded_predictions$prediction_id
  ) > 0L
) {
  stop(
    "Duplicate Mosqlimate prediction IDs were returned."
  )
}


cat(
  "\n============================================================\n",
  "UPLOADS COMPLETED\n",
  "============================================================\n",
  "Uploaded predictions: ",
  nrow(
    uploaded_predictions
  ),
  "\n",
  "Upload log: ",
  upload_log_path,
  "\n\n",
  sep = ""
)

print(
  uploaded_predictions,
  n = Inf
)


# =============================================================================
# 9. Verify uploaded predictions in Mosqlimate
# =============================================================================

check <- mosq$get_predictions(
  api_key = api_key,
  model_name = model_name,
  model_owner = model_owner,
  adm_level = 1L,
  disease = "A90",
  imdc_year = 2026L
)

check_index <- purrr::map_dfr(
  check,
  \(p) {
    tibble::tibble(
      id = as.integer(
        p$id
      ),
      adm_1 = as.integer(
        p$adm_1
      ),
      start = as.character(
        p$start
      ),
      end = as.character(
        p$end
      ),
      commit = as.character(
        p$commit
      )
    )
  }
)

new_production <- check_index |>
  dplyr::filter(
    start == as.character(
      expected_start
    ),
    end == as.character(
      expected_end
    )
  ) |>
  dplyr::arrange(
    adm_1
  )


if (
  nrow(
    new_production
  ) != 26L
) {
  print(
    new_production,
    n = Inf
  )
  
  stop(
    "Mosqlimate registry does not contain exactly 26 new production predictions."
  )
}

if (
  !setequal(
    new_production$adm_1,
    expected_uf_codes
  )
) {
  print(
    new_production,
    n = Inf
  )
  
  stop(
    "Mosqlimate production UF codes do not match the expected state set."
  )
}

if (
  !all(
    new_production$commit == commit
  )
) {
  print(
    new_production,
    n = Inf
  )
  
  stop(
    "At least one uploaded prediction is associated with the wrong commit."
  )
}


cat(
  "\n============================================================\n",
  "PRODUCTION SUBMISSION VERIFIED\n",
  "============================================================\n",
  "Predictions: 26\n",
  "Forecast period: ",
  expected_start,
  " to ",
  expected_end,
  "\n",
  "Commit: ",
  commit,
  "\n",
  "Total IMDC registry records returned: ",
  nrow(
    check_index
  ),
  "\n\n",
  sep = ""
)

print(
  new_production,
  n = Inf
)

cat(
  "\nDone.\n"
)
