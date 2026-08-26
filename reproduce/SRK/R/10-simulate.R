# =============================================================================
# 10-simulate.R — the paper's simulation experiment (Sec. 2.3, Table 1, Fig. 1-2)
#
# One autocorrelated response field and one covariate field are simulated on a
# 20 x 20 grid, then pushed through three transforms of increasing skew. Each
# scenario is split 70/30 and predicted twice: by ordinary kriging, and by SRK.
# The point of the experiment is that SRK's margin over OK grows as the
# distribution departs from Gaussian.
#
# The generating code is a direct port of Section 1 of the authors'
# 02_main_analysis.R, including the seeds.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("sp", "gstat", "automap", "ranger")
log_head("10  Simulation experiment")
t0 <- Sys.time()

# -- Two base fields ----------------------------------------------------------
set.seed(SEED)
xy <- expand.grid(x = seq_len(SIM_SIDE), y = seq_len(SIM_SIDE))
sp::gridded(xy) <- ~x + y

sim_field <- function(range) {
  g <- gstat::gstat(formula = z ~ 1, dummy = TRUE, beta = 0,
                    model = gstat::vgm(psill = 1, model = "Sph", range = range),
                    nmax = 20)
  stats::predict(g, newdata = xy, nsim = 1)@data$sim1
}
y_raw     <- sim_field(SIM_RANGE_Y)
noise_raw <- sim_field(SIM_RANGE_X)

# The covariate keeps a strong but imperfect association with the response.
x_raw  <- SIM_MIX * y_raw + (1 - SIM_MIX) * noise_raw
y_base <- y_raw + SIM_SHIFT          # shifted so the transforms stay positive
x_base <- x_raw + SIM_SHIFT
log_info("base fields: cor(X, Y) = %.3f", stats::cor(x_base, y_base))

# -- Three distributional scenarios ------------------------------------------
scenarios <- list(
  normal      = list(Y = y_base,           X = x_base),
  skewed      = list(Y = y_base^3 / 100,   X = x_base^3 / 100),
  `long-tail` = list(Y = exp(y_base)/1000, X = exp(x_base)/1000)
)

skewness <- function(v) mean((v - mean(v))^3) / stats::sd(v)^3
grid_df  <- as.data.frame(xy)

fields  <- list(); metrics <- list(); preds <- list(); imps <- list()

for (nm in names(scenarios)) {
  sc <- scenarios[[nm]]
  df <- data.frame(grid_df, y_obs = sc$Y, x_cov = sc$X)

  set.seed(SEED)
  tr_idx <- sample(seq_len(nrow(df)), floor(nrow(df) * SIM_TRAIN))
  tr <- df[tr_idx, ]; te <- df[-tr_idx, ]

  ok <- bench_ok(tr, te, "y_obs")
  # The simulation has a single covariate and the authors keep its singularity
  # feature unconditionally, so the SD filter is switched off here; the case
  # study in step 40 applies it at the published 0.5.
  sk <- srk_run(tr, te, "y_obs", "x_cov", scales = SV_SCALES_SIM,
                sd_threshold = 0, use_coords = FALSE)

  fields[[nm]]  <- data.frame(scenario = nm, df,
                              split = ifelse(seq_len(nrow(df)) %in% tr_idx,
                                             "train", "test"))
  metrics[[nm]] <- rbind(
    data.frame(scenario = nm, model = "OK",  srk_metrics(te$y_obs, ok$pred)),
    data.frame(scenario = nm, model = "SRK", srk_metrics(te$y_obs, sk$pred)))
  preds[[nm]]   <- data.frame(scenario = nm, x = te$x, y = te$y,
                              obs = te$y_obs, OK = ok$pred, SRK = sk$pred,
                              trend = sk$trend, kriged = sk$kriged)
  imps[[nm]]    <- data.frame(scenario = nm, feature = names(sk$importance),
                              importance = as.numeric(sk$importance))

  m <- metrics[[nm]]
  log_info("%-9s skew(Y) = %6.2f   OK R2 = %.3f   SRK R2 = %.3f",
           nm, skewness(sc$Y), m$R2[1], m$R2[2])
}

fields  <- do.call(rbind, fields)
metrics <- do.call(rbind, metrics)

# The singularity feature of the training samples, kept for Fig. 2's scatter.
sv_by_scenario <- do.call(rbind, lapply(names(scenarios), function(nm) {
  d <- fields[fields$scenario == nm, ]
  a <- srk_singularity(d$x, d$y, d$x_cov, d$x, d$y, SV_SCALES_SIM,
                       MIN_PTS_PER_SCALE, MIN_VALID_SCALES, neutral = SV_NEUTRAL)
  data.frame(d, sv_x_cov = a)
}))

write_result(sv_by_scenario, F_SIM_FIELDS)
write_result(metrics,        F_SIM_RESULTS)
write_result(do.call(rbind, preds), F_SIM_DETAIL)
write_result(do.call(rbind, imps),  F_SIM_IMP)

# -- How far the reproduction lands from the printed Table 1 -----------------
paper <- data.frame(
  scenario = rep(c("normal", "skewed", "long-tail"), each = 2),
  model    = rep(c("OK", "SRK"), 3),
  R2       = c(0.876, 0.972, 0.833, 0.966, 0.736, 0.940),
  RMSE     = c(0.361, 0.172, 0.361, 0.162, 0.178, 0.085),
  MAE      = c(0.284, 0.137, 0.265, 0.120, 0.107, 0.056))
cmp <- merge(metrics, paper, by = c("scenario", "model"),
             suffixes = c("", "_paper"))
cmp <- cmp[order(match(cmp$scenario, names(scenarios)), cmp$model), ]
log_info("R2 vs published Table 1 (this run / paper):")
for (i in seq_len(nrow(cmp)))
  log_info("  %-9s %-3s  %.3f / %.3f", cmp$scenario[i], cmp$model[i],
           cmp$R2[i], cmp$R2_paper[i])
write_result(cmp, file.path(RES_DIR, "simulation-vs-paper.csv"))

record_runtime("10-simulate", as.numeric(difftime(Sys.time(), t0, units = "secs")))
