setup_hpc_library <- function() {

  user <- Sys.info()[["user"]]

  hpc_lib <- file.path(
    "/gpfs/scratch/bsc32",
    user,
    "libraries"
  )

  dir.create(hpc_lib, recursive = TRUE, showWarnings = FALSE)

  .libPaths(c(hpc_lib, .libPaths()))

  invisible(.libPaths())
}