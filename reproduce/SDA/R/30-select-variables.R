# =============================================================================
# 30-select-variables.R — choose which second-dimension variables to keep
#
# Eight surfaces x 55 (b, tau) pairs = 440 candidates for 614 samples, and the
# candidates are strongly collinear by construction (neighbouring buffers and
# neighbouring quantiles describe almost the same neighbourhood). Selection
# ranks by |correlation with the response| and then walks down that order,
# dropping any variable that pushes the VIF above VIF_MAX.
#
# Outputs: results/selected-variables.csv   (the chosen variables)
#          results/selected-parameters.csv   (their b and tau, for the figure)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Step 3/5  Select second-dimension variables")

pts <- read_result(F_POINTS)
xall <- load_reference_vars()
log_info("candidates: %d surfaces x %d variables = %d", length(xall),
         ncol(xall[[1]]), length(xall) * ncol(xall[[1]]))

krm <- sda_rmoutlier(pts$y, OUTLIER_SD)
if (length(krm)) log_info("removed %d outlier(s) beyond %.1f SD", length(krm), OUTLIER_SD)
y <- if (length(krm)) pts$y[-krm] else pts$y
x <- if (length(krm)) lapply(xall, function(m) m[-krm, , drop = FALSE]) else xall

t0 <- Sys.time()
sel <- sda_select(y, x, ctr.vif = VIF_MAX)
el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
log_info("selected %d of %d variables in %.2fs", ncol(sel),
         length(xall) * ncol(xall[[1]]), el)

write_result(cbind(y = y, sel), F_SELECTED)

# -- Which (b, tau) did the data choose? --------------------------------------
# Column names look like Elevation_b5t0.7: surface, buffer, quantile.
parts <- do.call(rbind, lapply(names(sel), function(n) {
  m <- regmatches(n, regexec("^(.*)_b([0-9.]+)t([0-9.]+)$", n))[[1]]
  if (length(m) != 4) return(NULL)
  data.frame(variable = n, surface = m[2],
             buffer_km = as.numeric(m[3]), quantile = as.numeric(m[4]))
}))
parts$correlation <- round(vapply(parts$variable,
  function(n) stats::cor(y, sel[[n]]), numeric(1)), 4)
parts <- parts[order(-abs(parts$correlation)), ]
write_result(parts, F_SELPARAM)

log_info("surfaces represented: %s", paste(sort(unique(parts$surface)), collapse = ", "))
log_info("buffers chosen: %s km | quantiles chosen: %s",
         paste(sort(unique(parts$buffer_km)), collapse = ", "),
         paste(sort(unique(parts$quantile)), collapse = ", "))
log_info("strongest: %s (r = %.3f)", parts$variable[1], parts$correlation[1])
