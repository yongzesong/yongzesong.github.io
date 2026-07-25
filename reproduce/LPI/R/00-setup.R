# =============================================================================
# 00-setup.R — check and install required packages
# Run once: Rscript R/00-setup.R
# =============================================================================

cran_packages <- c(
  "yaml",          # config loading
  "sf",            # spatial data
  "sdsfun",        # spatial weights, discretize_vector
  "geocomplexity", # geocd_vector — GC patterns
  "gdverse",       # gd() — geographical detector q-statistic
  "FNN",           # k-nearest neighbours
  "rpart",         # GOZH decision tree
  "automap",       # autofitVariogram — local range
  "dplyr",
  "parallel",
  # figures
  "ggplot2", "reshape2", "mgcv", "scales", "patchwork", "ggrepel"
)

# localsp provides lisp(), the LISP factor detector.
# If unavailable on CRAN for your R version, install from GitHub:
#   remotes::install_github("stscl/localsp")
other_packages <- c("localsp")

missing <- setdiff(cran_packages, rownames(installed.packages()))
if (length(missing) > 0) {
  cat("Installing:", paste(missing, collapse = ", "), "\n")
  install.packages(missing)
}

for (pkg in other_packages) {
  if (!pkg %in% rownames(installed.packages())) {
    cat(sprintf("Package '%s' missing — trying CRAN, then GitHub.\n", pkg))
    tryCatch(install.packages(pkg), error = function(e) NULL)
    if (!pkg %in% rownames(installed.packages())) {
      if (!"remotes" %in% rownames(installed.packages())) {
        install.packages("remotes")
      }
      remotes::install_github(paste0("stscl/", pkg))
    }
  }
}

cat("\nAll packages present:\n")
for (pkg in c(cran_packages, other_packages)) {
  ok <- requireNamespace(pkg, quietly = TRUE)
  cat(sprintf("  %-14s %s\n", pkg, ifelse(ok, "OK", "MISSING")))
}

# Record the environment for reproducibility.
dir.create("results", showWarnings = FALSE)
writeLines(capture.output(sessionInfo()), "results/session-info.txt")
cat("\nSession info written to results/session-info.txt\n")
