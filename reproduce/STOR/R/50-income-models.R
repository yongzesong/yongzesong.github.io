# =============================================================================
# 50-income-models.R — contributions of Gamma_A and Gamma_B to income
# =============================================================================
# The paper's Section 3.4: a GAM (Eq. 10) reveals the nonlinear effects, a GWR
# (Eq. 11) the regional ones. The contribution of each dimension is the
# deviance explained that is lost when the dimension is dropped, averaged over
# the two models — the quantity behind Fig. 11a.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("50  GAM + GWR: income contribution")

srii   <- read_result(F_SRII)
blocks <- read_result(F_BLOCKS)
stopifnot(all(srii$block_id == blocks$block_id))
z <- blocks[[INCOME_COL]]

# -- GAM (mgcv ships with R) --------------------------------------------------
dat <- data.frame(z = z, ga = srii$gamma_a, gb = srii$gamma_b)
gam_full <- mgcv::gam(z ~ s(ga) + s(gb), data = dat, method = "REML")
gam_a    <- mgcv::gam(z ~ s(ga), data = dat, method = "REML")
gam_b    <- mgcv::gam(z ~ s(gb), data = dat, method = "REML")
dev_expl <- function(m) summary(m)$dev.expl
gam_contrib <- c(A = dev_expl(gam_full) - dev_expl(gam_b),
                 B = dev_expl(gam_full) - dev_expl(gam_a))
log_info("GAM deviance explained %.1f%% (Gamma_A %.1f%%, Gamma_B %.1f%%)",
         100 * dev_expl(gam_full), 100 * gam_contrib["A"], 100 * gam_contrib["B"])

# -- GWR (base-R implementation in 02-stor-core.R) ----------------------------
coords <- srii[, c("x_km", "y_km")]
X <- cbind(ga = srii$gamma_a, gb = srii$gamma_b)
gwr_full <- stor_gwr_select(z, X, coords, GWR_NEIGHBORS)
k <- gwr_full$k
gwr_a <- stor_gwr(z, X[, "ga", drop = FALSE], coords, k)
gwr_b <- stor_gwr(z, X[, "gb", drop = FALSE], coords, k)
gwr_contrib <- c(A = gwr_full$r2 - gwr_b$r2, B = gwr_full$r2 - gwr_a$r2)
log_info("GWR (adaptive k = %d) R2 %.3f (Gamma_A %.1f%%, Gamma_B %.1f%%)",
         k, gwr_full$r2, 100 * gwr_contrib["A"], 100 * gwr_contrib["B"])

write_result(data.frame(
  model = rep(c("GAM", "GWR", "Mean"), each = 2),
  dimension = rep(c("Gamma_A", "Gamma_B"), 3),
  contribution = c(gam_contrib, gwr_contrib,
                   (gam_contrib + gwr_contrib) / 2)), F_CONTRIB)

write_result(data.frame(srii[, c("block_id", "col", "row", "x_km", "y_km")],
                        beta0 = gwr_full$beta$Intercept,
                        beta_a = gwr_full$beta$ga,
                        beta_b = gwr_full$beta$gb,
                        fitted = gwr_full$fitted,
                        residual = gwr_full$residuals), F_GWRCOEF)
log_info("GWR bandwidth candidates: %s",
         paste(sprintf("k=%d AICc=%.1f", gwr_full$candidates$k,
                       gwr_full$candidates$aicc), collapse = "; "))
