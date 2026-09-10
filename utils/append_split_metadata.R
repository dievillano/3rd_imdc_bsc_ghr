append_split_metadata <- function(
    data,
    metadata,
    prefix = "."
) {
  
  if (is.null(metadata)) {
    return(data)
  }
  
  metadata <- tibble::as_tibble(metadata)
  
  if (nrow(metadata) != 1L) {
    rlang::abort(
      "`metadata` must contain exactly one row."
    )
  }
  
  metadata_values <- metadata |>
    dplyr::slice(1L) |>
    as.list()
  
  metadata_names <- names(metadata_values)
  
  # Avoid adding the prefix twice
  metadata_names <- ifelse(
    startsWith(metadata_names, prefix),
    metadata_names,
    paste0(prefix, metadata_names)
  )
  
  names(metadata_values) <- metadata_names
  
  dplyr::mutate(
    data,
    !!!metadata_values
  )
}