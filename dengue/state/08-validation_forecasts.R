args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Usage: Rscript <script_name>.R <split_id>")
}

split_id <- as.integer(args[[1]])

if (is.na(split_id)) {
  stop("split_id must be an integer.")
}

sprint2026_path <- here::here()

data_path <- fs::path(sprint2026_path, "data")
processed_data_path <- fs::path(data_path, "processed/health_region")
processed_graph_path <- fs::path(processed_data_path, "graph")
external_splits_path <- fs::path(processed_data_path, "external_splits")

dengue_path <- fs::path(sprint2026_path, "dengue/state")
outputs_path <- fs::path(dengue_path, "outputs")
predictions_path <- fs::path(outputs_path, "val_predictions")

fs::dir_create(predictions_path)

utils_path <- fs::path(sprint2026_path, "utils")
utils_filepaths <- fs::dir_ls(utils_path)
purrr::walk(utils_filepaths, source)

setup_hpc_library()

# 1. Read splits ----------------------------------------------------------

external_splits <- readRDS(
  fs::path(external_splits_path, "dengue_hr_external_splits.rds")
)

# 2. Model variables ------------------------------------------------------

climate_predictors <- c(
  "tasan6.l1",
  "spei3.l1",
  "spei12.l3",
  "tas6.l1",
  "oni.l6"
)

# 3. Formula environment --------------------------------------------------

hr_graph <- INLA::inla.read.graph(
  fs::path(processed_graph_path, "hr_graph.graph")
)

prec_prior <- list(
  prec = list(prior = "pc.prec", param = c(0.5, 0.01))
)

formula_env <- rlang::env(
  hr_graph = hr_graph,
  prec_prior = prec_prior
)

re_s <- paste(
  "f(hr_id, model = 'bym2', graph = hr_graph, scale.model = TRUE,",
  "hyper = prec_prior, constr = TRUE)"
)

re_w <- paste(
  "f(week_id, model = 'rw2', replicate = state_id, cyclic = TRUE,",
  "constr = TRUE, scale.model = TRUE, hyper = prec_prior)"
)

re_y <- "f(year_id, model = 'iid', hyper = prec_prior)"

# NOTE:
# This is an INLA-style approximation of the previous GHRmodel formula.
# If you already have cov_varying() / cov_nl() translated to INLA strings,
# replace this block with those exact terms.

formula_string <- paste(
  "cases ~ 1",
  "v1 + v2 + v3 + v1v2 + v1v3 + v2v3 + v1v2v3",
  "tas6.l1 + oni.l6 + period_id",
  re_s,
  re_w,
  re_y,
  sep = " + "
)

# 4. Run selected external split -----------------------------------------

if (!split_id %in% seq_along(external_splits)) {
  stop(
    "Invalid split_id: ", split_id,
    ". Available splits: ",
    paste(seq_along(external_splits), collapse = ", ")
  )
}

cat("\nRunning external split ", split_id, "\n", sep = "")

out_file <- fit_forecast_inla_split(
  split = external_splits[[split_id]],
  split_id = split_id,
  formula_string = formula_string,
  climate_predictors = climate_predictors,
  formula_env = formula_env,
  predictions_path = predictions_path,
  model_id = "legacy_model",
  model_label = "Legacy model"
)

cat("\nSaved prediction file reference:\n", out_file, "\n", sep = "")
