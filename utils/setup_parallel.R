setup_parallel <- function(
    mode = c("sequential", "local", "hpc"),
    workers = 1
) {
  mode <- match.arg(mode)
  
  if (mode == "sequential") {
    future::plan(future::sequential)
  }
  
  if (mode == "local") {
    future::plan(future::multisession, workers = workers)
  }
  
  if (mode == "hpc") {
    # Usually one R process per allocated core on a single node
    future::plan(future::multisession, workers = workers)
    
    # For multi-node HPC, use future.batchtools later if needed.
  }
}