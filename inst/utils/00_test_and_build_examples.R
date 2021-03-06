inputfiles <- dir("./data_files/parameters_validation", full.names = TRUE)

# run for all specs files
for (i in 4:9){#seq_along(inputfiles)){
  cat("\nProcessing example", i, "of", length(inputfiles), "\n")
  defs <- run_CALANGO(inputfiles[i], cores = parallel::detectCores() - 1)
}
