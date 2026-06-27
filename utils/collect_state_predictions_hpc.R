collect_state_predictions_hpc <- function(
    path,
    pattern = "\\.rds$",
    recursive = FALSE,
    verbose = TRUE
) {
  
  if (!fs::dir_exists(path)) {
    stop("Directory does not exist: ", path)
  }
  
  filepaths <- fs::dir_ls(
    path = path,
    regexp = pattern,
    recurse = recursive,
    type = "file"
  )
  
  if (length(filepaths) == 0) {
    stop("No state prediction files found in: ", path)
  }
  
  if (verbose) {
    cat("Found", length(filepaths), "state prediction files.\n")
  }
  
  predictions <- filepaths |>
    purrr::map(readRDS) |>
    purrr::list_rbind(names_to = ".file_id") |>
    dplyr::mutate(
      .source_file = fs::path_file(filepaths[.file_id]),
      .source_path = as.character(filepaths[.file_id]),
      .before = 1
    ) |>
    dplyr::select(-.file_id)
  
  predictions
}