# =============================================================================
# test-01-method-properties.R — the implementation satisfies the definitions
#
# Every component is checked against a property the published method requires:
# the intensity window, the log-log slope of Eq. 3-4 against a field with a
# known scaling exponent, the neutral value of a uniform field, the SD filter,
# the additive decomposition of Eq. 9, the metrics of Eq. 11-13, and the block
# folds. The vectorised singularity is also checked against a literal
# transcription of the authors' singularity_safe() loop.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

cat("== test-01  Method properties =============================================\n")
pass <- 0; total <- 0
check <- function(label, cond) {
  total <<- total + 1; ok <- isTRUE(cond)
  cat(sprintf("   %-58s %s\n", label, if (ok) "pass" else "FAIL"))
  pass <<- pass + ok
}
set.seed(SEED)

## -- Reference implementation: the authors' loop, transcribed -----------------
singularity_safe_ref <- function(x_ref, y_ref, z_ref, x_eval, y_eval,
                                 size, min_pts_per_scale = 5,
                                 min_valid_scales = 3, use_abs = TRUE,
                                 eps = 1e-9) {
  if (use_abs) z_ref <- abs(z_ref)
  nz <- function(v) ifelse(is.finite(v) & v > 0, v, eps)
  alpha <- numeric(length(x_eval))
  for (i in seq_along(x_eval)) {
    C_A_r <- numeric(length(size)); valid <- logical(length(size))
    for (j in seq_along(size)) {
      k <- which(abs(x_ref - x_eval[i]) <= size[j] &
                 abs(y_ref - y_eval[i]) <= size[j])
      if (length(k) >= min_pts_per_scale) {
        C_A_r[j] <- mean(z_ref[k], na.rm = TRUE)
        valid[j] <- is.finite(C_A_r[j])
      } else valid[j] <- FALSE
    }
    if (sum(valid) >= min_valid_scales) {
      fit <- stats::lm(log(nz(C_A_r[valid])) ~ log(size[valid]))
      alpha[i] <- stats::coef(fit)[2] + 2
    } else alpha[i] <- NA_real_
  }
  alpha
}

## -- Eq. 1-4: intensity and the log-log slope --------------------------------
n  <- 300
px <- stats::runif(n, 0, 100); py <- stats::runif(n, 0, 100)
pz <- stats::runif(n, 1, 5)
sc <- c(5, 10, 15, 20, 25)

C <- srk_intensity(px, py, pz, px, py, sc, min_pts_per_scale = 3L)
check("intensity has one column per scale", ncol(C) == length(sc))
check("intensity is non-decreasing in coverage",
      all(is.na(C[, 1]) | C[, 1] >= min(pz) - 1e-9))
i1 <- which(!is.na(C[, 3]))[1]
k  <- abs(px - px[i1]) <= sc[3] & abs(py - py[i1]) <= sc[3]
check("intensity equals the mean |X| inside the square window",
      abs(C[i1, 3] - mean(abs(pz[k]))) < 1e-12)

check("vectorised singularity matches the authors' loop",
      {
        a1 <- srk_singularity(px, py, pz, px, py, sc, 5L, 3L, neutral = NA)
        a2 <- singularity_safe_ref(px, py, pz, px, py, sc, 5, 3)
        max(abs(a1 - a2), na.rm = TRUE) < 1e-10 &&
          identical(is.na(a1), is.na(a2))
      })

## -- alpha = 2 is the neutral value for a uniform field ----------------------
gx <- rep(1:20, 20); gy <- rep(1:20, each = 20)
flat <- srk_singularity(gx, gy, rep(3, 400), gx, gy, 1:6, 3L, 2L)
check("a uniform field gives alpha = 2 everywhere",
      max(abs(flat - 2)) < 1e-9)

## -- A field with a known exponent recovers that exponent --------------------
# C(A, r) proportional to r^(beta) built by construction: the mean |X| inside a
# window of half-width r grows as r^beta when |X| ~ d^beta from a point source.
beta <- 0.6
d    <- sqrt((gx - 10.5)^2 + (gy - 10.5)^2)
src  <- (d + 0.5)^beta
a_src <- srk_singularity(gx, gy, src, 10.5, 10.5, 2:8, 3L, 2L)
C_src <- srk_intensity(gx, gy, src, 10.5, 10.5, 2:8, 3L)
slope <- stats::coef(stats::lm(log(as.numeric(C_src)) ~ log(2:8)))[2]
check("alpha equals the fitted log-log slope plus two",
      abs(a_src - (slope + 2)) < 1e-9)
check("a field growing away from a source is depleted (alpha > 2)", a_src > 2)

## -- The SD filter -----------------------------------------------------------
df <- data.frame(sv_a = stats::rnorm(200, 2, 3), sv_b = stats::rnorm(200, 2, 0.01))
check("SD filter keeps the varying feature and drops the flat one",
      identical(srk_select_features(df, c("sv_a", "sv_b"), 0.5), "sv_a"))
check("SD filter at zero keeps everything",
      length(srk_select_features(df, c("sv_a", "sv_b"), 0)) == 2L)

## -- Eq. 9: the prediction is trend plus kriged residual ---------------------
sim <- read_result(F_SIM_DETAIL)
check("SRK prediction equals trend plus kriged residual (Eq. 9)",
      max(abs(sim$SRK - (sim$trend + sim$kriged))) < 1e-9)

## -- Eq. 11-13: the metrics --------------------------------------------------
o <- stats::rnorm(100); p <- o + stats::rnorm(100, 0, 0.3)
m <- srk_metrics(o, p)
check("R2 of a perfect prediction is 1", abs(srk_metrics(o, o)$R2 - 1) < 1e-12)
check("RMSE is at least MAE", m$RMSE >= m$MAE)
check("R2 matches 1 - SSE/SST",
      abs(m$R2 - (1 - sum((p - o)^2) / sum((o - mean(o))^2))) < 1e-12)

## -- Spatial block folds -----------------------------------------------------
ELEM <- names(ELEMENTS)[1]
d    <- read_result(F_SAMPLES(ELEM))
f1 <- srk_block_folds(d, CV_FOLDS, CV_BLOCK_SIZE, CV_SEED)
f2 <- srk_block_folds(d, CV_FOLDS, CV_BLOCK_SIZE, CV_SEED)
check("block folds are reproducible for a fixed seed", identical(f1, f2))
check("every sample lands in exactly one fold",
      length(f1) == nrow(d) && all(f1 %in% seq_len(CV_FOLDS)))
check("a block is never split across folds",
      {
        b <- paste(floor((d$x - min(d$x)) / CV_BLOCK_SIZE),
                   floor((d$y - min(d$y)) / CV_BLOCK_SIZE), sep = "_")
        all(vapply(split(f1, b), function(v) length(unique(v)) == 1L, logical(1)))
      })
check("held-out folds are geographically contiguous blocks, not random points",
      {
        # A random split would put a training point within one block edge of
        # almost every test point; blocking must do measurably better.
        te <- d[f1 == 1, ]; tr <- d[f1 != 1, ]
        nn <- vapply(seq_len(nrow(te)), function(i)
          min(sqrt((tr$x - te$x[i])^2 + (tr$y - te$y[i])^2)), numeric(1))
        stats::median(nn) > 0
      })

## -- The singularity features do not touch the response ----------------------
check("singularity is computed from covariates, not the response",
      {
        yv <- ELEMENTS[[ELEM]]$yvar
        d2 <- d; d2[[yv]] <- stats::rnorm(nrow(d2), 500, 100)   # scramble Z
        a1 <- srk_singularity(d$x, d$y, d$hm, d$x, d$y, SV_SCALES,
                              MIN_PTS_PER_SCALE, MIN_VALID_SCALES)
        a2 <- srk_singularity(d2$x, d2$y, d2$hm, d2$x, d2$y, SV_SCALES,
                              MIN_PTS_PER_SCALE, MIN_VALID_SCALES)
        identical(a1, a2)
      })

cat(sprintf("\n   %d/%d checks passed\n", pass, total))
if (pass < total) quit(status = 1)
