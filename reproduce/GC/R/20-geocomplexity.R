# =============================================================================
# 20-geocomplexity.R — measure the local complexity of every variable
#
# Geocomplexity asks, at each location, how far the neighbourhood departs from
# smooth spatial dependence. High values mark places where a variable changes
# in a way its surroundings do not predict — and those are the places where a
# model fitted on the variable's value alone tends to be wrong.
#
# Outputs: results/geocomplexity.csv          (GC per variable, GC_METHOD)
#          results/gc-method-comparison.csv    (moran vs spvar vs shannon)
#          results/gc-method-correlation.csv   (do the methods agree?)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("geocomplexity", "geocomplexity")

log_head("Step 2/5  Compute geocomplexity")

x  <- sf::read_sf(file.path(DERIVED, "analysis-layer.gpkg"))
wt <- readRDS(file.path(DERIVED, "weights.rds"))
preds <- predictor_names(x)
xp <- x[, preds]

## -- the working geocomplexity ------------------------------------------------
gc <- geocomplexity::geocd_vector(xp, wt = wt, method = GC_METHOD,
                                  normalize = GC_NORMALIZE, returnsf = FALSE)
log_info("method '%s': %d variables -> %d GC columns", GC_METHOD, length(preds), ncol(gc))
gc_df <- as.data.frame(gc)
write_result(gc_df, F_GC)

rng <- vapply(gc_df, function(v) c(min(v), max(v)), numeric(2))
log_info("GC range across variables: %.3f to %.3f", min(rng[1, ]), max(rng[2, ]))

## -- do the three measures tell the same story? -------------------------------
# moran reads local autocorrelation, spvar the local fluctuation, shannon the
# entropy of the neighbourhood. They are different questions, and a variable
# can be complex under one and simple under another.
cmp <- list()
for (m in GC_METHODS_COMPARED) {
  g <- try(geocomplexity::geocd_vector(xp, wt = wt, method = m,
                                       normalize = GC_NORMALIZE, returnsf = FALSE),
           silent = TRUE)
  if (inherits(g, "try-error")) { log_warn("method '%s' failed; skipped", m); next }
  g <- as.data.frame(g)
  cmp[[m]] <- data.frame(method = m, variable = preds,
                         mean_gc = round(colMeans(g), 4),
                         sd_gc   = round(vapply(g, stats::sd, numeric(1)), 4),
                         row.names = NULL)
  log_info("method '%-7s' mean GC over all variables = %.4f", m, mean(colMeans(g)))
}
cmp_df <- do.call(rbind, cmp)
write_result(cmp_df, F_GCMETHOD)

# Agreement between methods, per variable: if these are low, the choice of
# method is a modelling decision that has to be reported.
if (length(cmp) > 1) {
  mats <- lapply(GC_METHODS_COMPARED, function(m) {
    g <- try(geocomplexity::geocd_vector(xp, wt = wt, method = m,
                                         normalize = GC_NORMALIZE, returnsf = FALSE),
             silent = TRUE)
    if (inherits(g, "try-error")) NULL else as.data.frame(g)
  })
  names(mats) <- GC_METHODS_COMPARED
  mats <- Filter(Negate(is.null), mats)
  pairs <- utils::combn(names(mats), 2, simplify = FALSE)
  corr <- do.call(rbind, lapply(pairs, function(p) {
    data.frame(method1 = p[1], method2 = p[2], variable = preds,
               correlation = round(vapply(seq_along(preds), function(j)
                 stats::cor(mats[[p[1]]][[j]], mats[[p[2]]][[j]]), numeric(1)), 4),
               row.names = NULL)
  }))
  write_result(corr, F_GCCORR)
  for (p in pairs) {
    v <- corr$correlation[corr$method1 == p[1] & corr$method2 == p[2]]
    log_info("%s vs %s: correlation %.2f to %.2f (median %.2f)",
             p[1], p[2], min(v), max(v), stats::median(v))
  }
}
