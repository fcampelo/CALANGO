make_correlation_report <- function(defs){

  # ================== Prepare report ==================
  sumY    <- sapply(defs$y, sum)  # faster than apply() or colSums()!

  # Ignore these, they're only here to initialize certain packages
  # NOTE: these are *important*, for some weird reason the report generation
  # breaks if you don't initialise (at least) the dendextend package.
  tmp <- dendextend::fac2num(factor(3:5))
  tmp <- plotly::hobbs
  tmp <- heatmaply::BrBG(5)

  # filter out those with no observations (sum equals zero)
  idx     <- (sumY != 0)
  Y       <- defs$y[, idx]
  sumY    <- sumY[idx]
  defs$sd <- defs$sd[idx]
  defs$cv <- defs$cv[idx]

  defs$greaterthanzero <- defs$greaterthanzero[idx]
  defs$heterogeneity   <- defs$heterogeneity[idx]

  # Prepare plotframe
  plotframe <- rbind(defs$contrasts.corrected[order(names(defs$contrasts.corrected))],
                     defs$results.correlations.pvalue.pearson[order(names(defs$results.correlations.pvalue.pearson))],
                     defs$results.correlations.pvalue.spearman[order(names(defs$results.correlations.pvalue.spearman))],
                     defs$results.correlations.pvalue.kendall[order(names(defs$results.correlations.pvalue.kendall))],
                     defs$correlations.pearson[order(names(defs$correlations.pearson))],
                     defs$correlations.spearman[order(names(defs$correlations.spearman))],
                     defs$correlations.kendall[order(names(defs$correlations.kendall))],
                     sumY[order(names(sumY))],
                     defs$sd[order(names(defs$sd))],
                     defs$cv[order(names(defs$cv))],
                     defs$greaterthanzero[order(names(defs$greaterthanzero))],
                     #                     defs$prevalence_per_group[order(names(defs$prevalence_per_group))],
                     defs$heterogeneity[order(names(defs$heterogeneity))])

  plotframe <- rbind(plotframe,
                     Y[, order(colnames(Y))])

  rownames(plotframe)[1:12] <- c("corrected_contrasts",
                                 "Pearson_qvalue",
                                 "Spearman_qvalue",
                                 "Kendall_qvalue",
                                 "Pearson_cor",
                                 "Spearman_cor",
                                 "Kendall_cor",
                                 "size",
                                 "sd",
                                 "cv",
                                 "prevalence",
                                 #                                 "prevalence per group",
                                 "heterogeneity")

  plotframe <- as.data.frame(t(plotframe))

  description           <- unlist(defs$annotation.cor)
  plotframe$description <- description[order(names(description))]
  plotframe$name        <- rownames(plotframe)

  idx <- which(
    # q-value cutoffs
    (plotframe$corrected_contrasts < defs$linear_model.qvalue.cutoff) &
      (plotframe$Spearman_qvalue   < defs$spearman.qvalue.cutoff) &
      (plotframe$Pearson_qvalue    < defs$pearson.qvalue.cutoff) &
      (plotframe$Kendall_qvalue    < defs$kendall.qvalue.cutoff) &
      # correlation cutoffs
      ((plotframe$Pearson_cor  > defs$pearson.cor.upper.cutoff)  | (plotframe$Pearson_cor  < defs$pearson.cor.lower.cutoff)) &
      ((plotframe$Spearman_cor > defs$spearman.cor.upper.cutoff) | (plotframe$Spearman_cor < defs$spearman.cor.lower.cutoff)) &
      ((plotframe$Kendall_cor  > defs$kendall.cor.upper.cutoff)  | (plotframe$Kendall_cor  < defs$kendall.cor.lower.cutoff)) &
      # basic statistics cutoffs
      (plotframe$size              > defs$annotation_size.cutoff) &
      (plotframe$prevalence        > defs$prevalence.cutoff) &
      (plotframe$heterogeneity     > defs$heterogeneity.cutoff) &
      (plotframe$sd                > defs$sd.cutoff) &
      (plotframe$cv                > defs$cv.cutoff))

  df_cutoff <- plotframe[idx, ]

  if (isTRUE(defs$raw_data_sd_filter)) {
    # remove trivial cases, constant values.
    df_cutoff <- df_cutoff[df_cutoff$sd != 0, ]
  }

  defs$sig_IDs <- rownames(df_cutoff)

  cat("\nGenerating HTML5 report for results")
  cat("\n-----------------------------------")
  cat("\nUsing filters:\n")

  cat("\ncorrected_contrasts <", sprintf("%.2f", defs$linear_model.qvalue.cutoff))
  if(defs$linear_model.qvalue.cutoff >= 1) {
    cat("\t\t(no filtering)")
  }

  cat("\nSpearman_qvalue     <", sprintf("%.2f", defs$spearman.qvalue.cutoff))
  if(defs$spearman.qvalue.cutoff >= 1) {
    cat("\t\t(no filtering)")
  }

  cat("\nPearson_qvalue      <", sprintf("%.2f", defs$pearson.qvalue.cutoff))
  if(defs$pearson.qvalue.cutoff >= 1) {
    cat("\t\t(no filtering)")
  }

  cat("\nKendall_qvalue      <", sprintf("%.2f", defs$kendall.qvalue.cutoff))
  if(defs$kendall.qvalue.cutoff >= 1) {
    cat("\t\t(no filtering)")
  }

  cat("\nPearson_cor         <", sprintf("%.2f", defs$pearson.cor.lower.cutoff),
      "[OR] >", sprintf("%.2f", defs$pearson.cor.upper.cutoff))
  if(defs$pearson.cor.lower.cutoff > defs$pearson.cor.upper.cutoff) {
    cat("\t(no filtering)")
  }

  cat("\nSpearman_cor        <", sprintf("%.2f", defs$spearman.cor.lower.cutoff),
      "[OR] >", sprintf("%.2f", defs$spearman.cor.upper.cutoff))
  if(defs$spearman.cor.lower.cutoff > defs$spearman.cor.upper.cutoff) {
    cat("\t(no filtering)")
  }

  cat("\nKendall_cor         <", sprintf("%.2f", defs$kendall.cor.lower.cutoff),
      "[OR] >", sprintf("%.2f", defs$kendall.cor.upper.cutoff))
  if(defs$kendall.cor.lower.cutoff > defs$kendall.cor.upper.cutoff) {
    cat("\t(no filtering)")
  }

  cat("\nsize                >", sprintf("%02d", defs$annotation_size.cutoff))
  if(defs$annotation_size.cutoff <= 0) {
    cat("\t\t(no filtering)")
  }

  cat("\nprevalence          >", sprintf("%.2f", defs$prevalence.cutoff))
  if(defs$prevalence.cutoff <= 0) {
    cat("\t\t(no filtering)")
  }

  cat("\nheterogeneity       >", sprintf("%.2f", defs$heterogeneity.cutoff))
  if(defs$heterogeneity.cutoff <= 0) {
    cat("\t\t(no filtering)")
  }

  cat("\nsd                  >", sprintf("%.2f", defs$sd.cutoff))
  if(defs$sd.cutoff <= 0) {
    cat("\t\t(no filtering)")
  }

  cat("\ncv                  >", sprintf("%.2f", defs$cv.cutoff))
  if(defs$cv.cutoff <= 0) {
    cat("\t\t(no filtering)")
  }

  cat("\nraw_data_sd_filter  =", defs$raw_data_sd_filter)
  cat("\n-----------------------------------")
  cat("\nThis may take a while...")


  # Prepare output folder

  #  od <- defs$output.dir

  #Uncomment the line below to generate full paths. Results may not work in servers.
  od  <- normalizePath(defs$output.dir)

  cpd <- gsub("//", "/", paste0(od, "/correlation_Plots/"),
              fixed = TRUE)

  if(!dir.exists(cpd)) dir.create(cpd, recursive = TRUE)

  # Copy report template into output dir
  files_to_copy <- dir(system.file("extdata", package = "KOMODO2"))
  fp <- files_to_copy

  for (i in seq_along(files_to_copy)){
    fp[i] <- gsub("//", "/", paste0(defs$output.dir, "/", files_to_copy[i]),
                  fixed = TRUE)
    file.copy(system.file("extdata", files_to_copy[i], package = "KOMODO2"),
              to = fp[i], overwrite = TRUE)
  }

  suppressWarnings(rmarkdown::render_site(input = defs$output.dir,
                                          quiet = TRUE))
  file.remove(fp)

  cat("\nAnd we're done!")

  invisible(defs)
}
