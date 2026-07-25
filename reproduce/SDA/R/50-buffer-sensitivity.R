# =============================================================================
# 50-buffer-sensitivity.R — how far outside the samples is worth looking?
#
# The searching range b is the one parameter that defines "outside". This step
# rebuilds the SDA model using one buffer at a time, so the manuscript can say
# which range carries the signal instead of asserting a default.
#
# Output: results/buffer-sensitivity.csv
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Step 5/5  Sensitivity to the searching range b")

pts  <- read_result(F_POINTS)
xall <- load_reference_vars()

set.seed(SEED)
train <- sample(nrow(pts), floor(TRAIN_FRACTION * nrow(pts)), replace = FALSE)
krm <- sda_rmoutlier(pts$y[train], OUTLIER_SD)
keep <- if (length(krm)) train[-krm] else train
ytr <- pts$y[keep]; yte <- pts$y[-train]

rows <- list()
for (b in DIST_BUFFERS) {
  # Keep only the columns generated at this buffer, across every surface.
  pick <- function(m) {
    cols <- grep(sprintf("^b%gt", b), names(m))
    m[, cols, drop = FALSE]
  }
  xtr <- lapply(xall, function(m) pick(m)[keep, , drop = FALSE])
  xte <- lapply(xall, function(m) pick(m)[-train, , drop = FALSE])
  if (ncol(xtr[[1]]) == 0) { log_warn("no columns for b = %g km", b); next }

  sel <- sda_select(ytr, xtr, ctr.vif = VIF_MAX)
  new <- sda_newdata(xte)[, names(sel), drop = FALSE]
  fit <- stats::lm(y ~ ., cbind(y = ytr, sel))
  p <- as.numeric(stats::predict(fit, newdata = new))
  rows[[length(rows) + 1]] <- data.frame(
    buffer_km = b, n_selected = ncol(sel),
    R2 = round(r2_score(yte, p), 4), RMSE = round(rmse_score(yte, p), 4))
  log_info("b = %2g km: %2d variables selected, R2 = %.4f", b, ncol(sel), r2_score(yte, p))
}

bs <- do.call(rbind, rows)
write_result(bs, F_BUFFER)
best <- bs$buffer_km[which.max(bs$R2)]
log_info("best single searching range: b = %g km (R2 = %.4f)", best, max(bs$R2))
log_info("range of R2 across buffers: %.4f to %.4f", min(bs$R2), max(bs$R2))
