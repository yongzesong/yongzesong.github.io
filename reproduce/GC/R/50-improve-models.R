# =============================================================================
# 50-improve-models.R — the practical consequence
#
# If geocomplexity explains what a model got wrong, then giving the model that
# information should make it less wrong. Two ways to do it:
#   GCMLR   — geocomplexity of each variable added as extra explanatory variables
#   GeoCGWR — geocomplexity built into the geographical weighting itself
#
# Output: results/model-improvement.csv
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Step 5/5  Geocomplexity improves the models")

x  <- sf::read_sf(file.path(DERIVED, "analysis-layer.gpkg"))
wt <- readRDS(file.path(DERIVED, "weights.rds"))
gc <- read_result(F_GC)
d  <- sf::st_drop_geometry(x)
preds <- predictor_names(x)
f <- stats::as.formula(paste(RESPONSE, "~", paste(preds, collapse = " + ")))

rows <- list()

## -- geocomplexity as extra explanatory variables -----------------------------
if (isTRUE(RUN_GCMLR)) {
  m1 <- stats::lm(f, data = d)
  m2 <- stats::lm(stats::as.formula(
          paste(RESPONSE, "~", paste(c(preds, names(gc)), collapse = " + "))),
          data = cbind(d, gc))
  rows[["MLR"]]   <- data.frame(model = "MLR",   variant = "baseline",
                                R2 = round(summary(m1)$r.squared, 4),
                                AdjR2 = round(summary(m1)$adj.r.squared, 4))
  rows[["GCMLR"]] <- data.frame(model = "GCMLR", variant = "geocomplexity",
                                R2 = round(summary(m2)$r.squared, 4),
                                AdjR2 = round(summary(m2)$adj.r.squared, 4))
  log_info("MLR    R2 = %.4f  AdjR2 = %.4f", summary(m1)$r.squared, summary(m1)$adj.r.squared)
  log_info("GCMLR  R2 = %.4f  AdjR2 = %.4f  (+%.1f%% R2)",
           summary(m2)$r.squared, summary(m2)$adj.r.squared,
           (summary(m2)$r.squared / summary(m1)$r.squared - 1) * 100)
}

## -- geocomplexity inside the geographical weighting --------------------------
if (isTRUE(RUN_GEOCGWR)) {
  if (!has_pkg("GWmodel")) {
    log_warn("GWmodel not installed; skipping the GWR baseline")
  } else {
    sp <- methods::as(x, "Spatial")
    bw <- GWmodel::bw.gwr(f, data = sp, approach = GWR_APPROACH,
                          kernel = GWR_KERNEL, adaptive = GWR_ADAPTIVE)
    g1 <- GWmodel::gwr.basic(f, data = sp, bw = bw,
                             kernel = GWR_KERNEL, adaptive = GWR_ADAPTIVE)
    rows[["GWR"]] <- data.frame(model = "GWR", variant = "baseline",
                                R2 = round(g1$GW.diagnostic$gw.R2, 4),
                                AdjR2 = round(g1$GW.diagnostic$gwR2.adj, 4))
    log_info("GWR      R2 = %.4f  AdjR2 = %.4f  (bandwidth %d)",
             g1$GW.diagnostic$gw.R2, g1$GW.diagnostic$gwR2.adj, bw)
  }
  g2 <- geocomplexity::gwr_geoc(f, data = x, bw = GWR_APPROACH,
                                adaptive = GWR_ADAPTIVE, kernel = GWR_KERNEL)
  rows[["GeoCGWR"]] <- data.frame(model = "GeoCGWR", variant = "geocomplexity",
                                  R2 = round(g2$diagnostic$R2, 4),
                                  AdjR2 = round(g2$diagnostic$R2_Adj, 4))
  log_info("GeoCGWR  R2 = %.4f  AdjR2 = %.4f", g2$diagnostic$R2, g2$diagnostic$R2_Adj)
}

imp <- do.call(rbind, rows)
write_result(imp, F_IMPROVE)

for (pair in list(c("MLR", "GCMLR"), c("GWR", "GeoCGWR"))) {
  a <- imp$R2[imp$model == pair[1]]; b <- imp$R2[imp$model == pair[2]]
  if (length(a) && length(b))
    log_info("%s -> %s: R2 %.4f -> %.4f (%+.1f%%)", pair[1], pair[2], a, b, (b/a - 1) * 100)
}
