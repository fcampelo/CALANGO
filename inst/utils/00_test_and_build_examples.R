# retrieve_data_files(target.dir = "data") # uncomment if needed

inputfiles <- dir("data/parameters/", full.names = TRUE)

ncores <- 1# parallel::detectCores() - 1

# run for all specs files
for (i in seq_along(inputfiles)){
  cat("\nProcessing example", i, "of", length(inputfiles), "\n")
  defs <- run_CALANGO(inputfiles[i], cores = ncores)
}