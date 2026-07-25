# =============================================================================
# 10-prepare-data.R — load the analysis table and report the variable roles
#
# Output: data/derived/analysis-data.csv   (the working copy)
#         results/variable-summary.csv      (role + type of every column)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Step 1/7  Prepare data")

d <- load_dataset()
if (is.null(INPUT_FILE)) {
  log_info("loaded built-in GD dataset '%s'", DATASET)
} else {
  log_info("loaded input file '%s'", INPUT_FILE)
}

need <- c(RESPONSE, CATEGORICAL, CONTINUOUS)
missing <- setdiff(need, names(d))
if (length(missing))
  stop("dataset lacks configured columns: ", paste(missing, collapse = ", "))

d <- d[, need]
log_info("%d rows, %d columns", nrow(d), ncol(d))
log_info("response: %s", RESPONSE)
log_info("%d categorical: %s", length(CATEGORICAL), paste(CATEGORICAL, collapse = ", "))
log_info("%d continuous: %s", length(CONTINUOUS), paste(CONTINUOUS, collapse = ", "))

role <- function(v) {
  if (v == RESPONSE) "response"
  else if (v %in% CATEGORICAL) "categorical"
  else "continuous"
}
summ <- data.frame(
  variable = need,
  role     = vapply(need, role, character(1)),
  n_unique = vapply(need, function(v) length(unique(d[[v]])), integer(1)),
  min      = vapply(need, function(v) if (is.numeric(d[[v]])) round(min(d[[v]], na.rm = TRUE), 3) else NA, numeric(1)),
  max      = vapply(need, function(v) if (is.numeric(d[[v]])) round(max(d[[v]], na.rm = TRUE), 3) else NA, numeric(1)),
  row.names = NULL
)
write_result(summ, F_VARS)
write_result(d, F_ANALYSIS)
log_info("analysis set: %d units, %d explanatory variables",
         nrow(d), length(CATEGORICAL) + length(CONTINUOUS))
