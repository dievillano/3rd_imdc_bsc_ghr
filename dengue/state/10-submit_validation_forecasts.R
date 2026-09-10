# 10-submit_validation_predictions.R

library(reticulate)
library(data.table)
library(dotenv)

dotenv::load_dot_env(".env")

api_key <- Sys.getenv("API_KEY")

reticulate::use_python(
  "/home/dvilla/.virtualenvs/imdc-mosq/bin/python",
  required = TRUE
)

reticulate::py_config()

mosq <- reticulate::import("mosqlient")

repository <- "dievillano/3rd_imdc_bsc_ghr"
commit <- "b9e77e94ce25f578474809883ed4d622dacf1bd2"

submission_path <- "dengue/state/outputs/val_submission/state_validation_submission.csv"

submission <- readr::read_csv(submission_path, show_col_types = FALSE)

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

required_cols <- c(".split_id", "uf_code", prediction_cols)

missing_cols <- setdiff(required_cols, names(submission))

if (length(missing_cols) > 0) {
  stop("Missing columns: ", paste(missing_cols, collapse = ", "))
}

submission <- submission |>
  dplyr::mutate(
    date = as.character(as.Date(date))
  )

upload_one_prediction <- function(data, split_id, uf_code) {
  
  prediction <- data |>
    dplyr::arrange(date) |>
    dplyr::select(dplyr::all_of(prediction_cols))
  
  description <- paste0(
    "Validation split ", split_id,
    " state-level dengue forecasts from BSC-GHR legacy model."
  )
  
  cat(
    "Uploading split ", split_id,
    ", state ", uf_code,
    ", ", nrow(prediction), " weeks...\n",
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

submission |>
  dplyr::arrange(.split_id, uf_code, date) |>
  dplyr::group_by(.split_id, uf_code) |>
  dplyr::group_walk(
    \(data, key) {
      upload_one_prediction(
        data = data,
        split_id = key$.split_id,
        uf_code = key$uf_code
      )
    }
  )

cat("Done.\n")