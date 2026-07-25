# =============================================================================
# 10-simulate-data.R — a synthetic study area with a known trade-off inside
# =============================================================================
# GRID_NX x GRID_NY square blocks. An urbanisation field (one main city, one
# secondary town, smooth spatial noise) drives the quantity of roads; quality
# follows the ground-truth utility curve of the config (logistic rise, then a
# decline past TRUE_T2), so the pipeline should recover boundaries near
# TRUE_T1 and TRUE_T2. Income depends on both dimensions with coefficients
# that drift across the map, which the GWR step should detect.
#
# Replace this single step with your own BLOCK_FILE to use real data.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("10  Simulate block data (%d x %d blocks)", GRID_NX, GRID_NY)
set.seed(SEED)

# -- Geometry -----------------------------------------------------------------
g <- expand.grid(col = seq_len(GRID_NX), row = seq_len(GRID_NY))
g$block_id <- seq_len(nrow(g))
g$x_km <- (g$col - 0.5) * CELL_KM
g$y_km <- (g$row - 0.5) * CELL_KM
n <- nrow(g)

# -- Smooth spatial noise: white noise convolved with a Gaussian kernel -------
smooth_field <- function(sigma = 2) {
  z <- matrix(rnorm(GRID_NX * GRID_NY), GRID_NX, GRID_NY)
  r <- ceiling(3 * sigma)
  off <- expand.grid(dc = -r:r, dr = -r:r)
  kw <- exp(-(off$dc^2 + off$dr^2) / (2 * sigma^2))
  out <- matrix(0, GRID_NX, GRID_NY)
  for (c0 in seq_len(GRID_NX)) for (r0 in seq_len(GRID_NY)) {
    cc <- c0 + off$dc; rr <- r0 + off$dr
    ok <- cc >= 1 & cc <= GRID_NX & rr >= 1 & rr <= GRID_NY
    out[c0, r0] <- sum(kw[ok] * z[cbind(cc[ok], rr[ok])]) / sum(kw[ok])
  }
  s <- as.numeric(out)[g$block_id]
  (s - mean(s)) / sd(s)
}

# -- Urbanisation and the two latent dimensions -------------------------------
d_city <- sqrt((g$x_km - 195)^2 + (g$y_km - 205)^2)   # main city, near centre
d_town <- sqrt((g$x_km - 320)^2 + (g$y_km - 80)^2)    # secondary town
U <- exp(-d_city / 45) + 0.35 * exp(-d_town / 40) + 0.14 * smooth_field(2.2)

# Rank-based Beta transform: keeps the spatial pattern (city gradient, smooth
# noise) but gives quantity a right-skewed distribution with support on the
# whole [0, 1] range, as in Fig. 6a of the paper.
q_true <- stats::qbeta((rank(U + rnorm(n, 0, 0.02)) - 0.5) / n, 1.15, 2.6)
q_true <- (q_true - min(q_true)) / (max(q_true) - min(q_true))
U <- q_true                                            # urbanisation == quantity

u_true <- function(t)
  U_BASE + U_AMP * stats::plogis((t - TRUE_T1) / U_SLOPE) + U_LIN * t -
  U_DECLINE * pmax(t - TRUE_T2, 0)^1.5

qual_true <- u_true(q_true) + 0.05 * smooth_field(2.5) + rnorm(n, 0, 0.012)
qual_true <- pmin(1, pmax(0, qual_true))

# -- Indicator variables (Table 1 of the paper, simplified) -------------------
clamp <- function(x, lo) pmax(x, lo)
blocks <- data.frame(
  g[, c("block_id", "col", "row", "x_km", "y_km")],
  road_density         = clamp(0.5 + 7.5 * q_true + rnorm(n, 0, 0.15), 0.05),
  main_road_density    = clamp(0.05 + 0.9 * q_true + rnorm(n, 0, 0.02), 0.005),
  roads_per_person     = clamp(0.2 + 2.5 * q_true + rnorm(n, 0, 0.06), 0.02),
  intersection_density = clamp(40 * q_true + rnorm(n, 0, 0.8), 0),
  roughness            = 1500 - 600 * qual_true + rnorm(n, 0, 20),
  rutting              = 900 - 350 * qual_true + rnorm(n, 0, 14),
  deflection           = 700 - 260 * qual_true + rnorm(n, 0, 11),
  curvature            = 400 - 150 * qual_true + rnorm(n, 0, 7),
  crash_rate           = clamp(8 - 5 * qual_true + rnorm(n, 0, 0.18), 0.2),
  craf_schools         = pmin(1, clamp(0.35 + 0.55 * qual_true + rnorm(n, 0, 0.015), 0.05)),
  craf_hospitals       = pmin(1, clamp(0.30 + 0.55 * qual_true + rnorm(n, 0, 0.02), 0.05)),
  craf_industry        = pmin(1, clamp(0.32 + 0.50 * qual_true + rnorm(n, 0, 0.02), 0.05)),
  dist_ports           = clamp(260 - 180 * qual_true + rnorm(n, 0, 6), 5),
  dist_airports        = clamp(150 - 100 * qual_true + rnorm(n, 0, 4), 3)
)

# -- Population and income ----------------------------------------------------
blocks$population <- round(15 + 4800 * U^2.2 * exp(rnorm(n, 0, 0.4)))

beta_a <- 0.5 + 1.0 * g$x_km / max(g$x_km)        # quantity payoff grows eastward
beta_b <- 35 + 20 * g$y_km / max(g$y_km)      # quality payoff grows northward
blocks$income <- 38 + beta_a * q_true + beta_b * qual_true + rnorm(n, 0, 1.5)

write_result(blocks, F_BLOCKS)
write_result(data.frame(block_id = g$block_id, urbanisation = U,
                        q_true = q_true, qual_true = qual_true,
                        beta_a = beta_a, beta_b = beta_b), F_TRUTH)
log_info("embedded DMU boundaries: t1 = %.3f, t2 = %.3f", TRUE_T1, TRUE_T2)
write_session_info()
