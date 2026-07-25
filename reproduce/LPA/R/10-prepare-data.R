# =============================================================================
# 10-prepare-data.R — read the point table, project it, describe what is in it
# =============================================================================
# Writes data/derived/points.rds and results/variable-summary.csv.
# =============================================================================

log_head("Step 10 — prepare data")

d <- read_points()
log_info("%d locations, %d columns, coordinates in %s",
         nrow(d), ncol(d), attr(d, "units"))
log_info("extent: %.2f to %.2f E, %.2f to %.2f N",
         min(d[[COORD_X]]), max(d[[COORD_X]]),
         min(d[[COORD_Y]]), max(d[[COORD_Y]]))
log_info("plane : %.0f km east-west by %.0f km north-south",
         diff(range(d$px)), diff(range(d$py)))

missing_vars <- setdiff(SEM_VARS, names(d))
if (length(missing_vars))
  stop("Input file is missing model variables: ", paste(missing_vars, collapse = ", "))

# -- What the observed variables look like ------------------------------------
# Every variable arrives standardised, so the summary is a check that the file
# is the one the model expects, not a description of raw physical units.
vs <- do.call(rbind, lapply(SEM_VARS, function(v) {
  x <- d[[v]]
  data.frame(variable = v, n = sum(is.finite(x)),
             mean = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE),
             min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE),
             stringsAsFactors = FALSE)
}))
vs$role <- c("Plant indicator (reference)", "Plant indicator",
             "Climate indicator", "Climate indicator",
             "Climate indicator (reference)",
             "Soil indicator (reference)", "Soil indicator")
write_result(vs, F_VARS)
print(vs, row.names = FALSE, digits = 3)

# -- What the published output looks like -------------------------------------
# The same file carries the authors' own estimates. Their gaps are part of the
# result: a location whose local model did not converge has no lambda at all.
pub_cols <- PATHS$published
sig_cols <- paste0("sig.", pub_cols)
have <- all(c(pub_cols, sig_cols) %in% names(d))
if (have) {
  n_complete_lambda <- sum(stats::complete.cases(d[, pub_cols]))
  n_complete_p      <- sum(stats::complete.cases(d[, sig_cols]))
  log_info("published lambda present at %d of %d locations (%d incomplete)",
           n_complete_lambda, nrow(d), nrow(d) - n_complete_lambda)
  log_info("published p-values complete at %d locations — Table 2's denominator",
           n_complete_p)
  outside <- sapply(pub_cols, function(cc) sum(abs(d[[cc]]) > 1, na.rm = TRUE))
  log_info("estimates outside [-1, 1]: %s",
           paste(sprintf("%s=%d", pub_cols, outside), collapse = ", "))
} else {
  log_warn("published lambda columns not found — step 40 will be skipped")
}

saveRDS(d, F_POINTS)
log_info("wrote data/derived/points.rds")
write_session_info()
