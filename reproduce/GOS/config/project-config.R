# =============================================================================
# project-config.R — the ONLY file you edit to re-run this pipeline on
#                    different data or with different settings.
# =============================================================================
# Everything below is read by R/00-config.R and used by every pipeline step.
# After editing this file, run:  Rscript run-all.R
# =============================================================================


# -- 1. Project identity ------------------------------------------------------
PROJECT_ID    <- "gos-zn"
PROJECT_TITLE <- "Geographically optimal similarity prediction of soil Zn"
DOMAIN        <- "trace element (Zn) concentration, Leonora mining region, WA"
METHOD_PAPER  <- "Song, Y. (2022) Math Geosci 55:295-320, doi:10.1007/s11004-022-10036-8"


# -- 2. Input data ------------------------------------------------------------
# Both tables ship with the geosimilarity package, so nothing is downloaded.
#   zn   : 894 geochemical samples, Lon/Lat + Zn + 9 covariates
#   grid : 13,132 prediction cells at 1 km, Lon/Lat + the same 9 covariates
# Step 10 writes verbatim CSV copies of both into data/ for transparency.
RESPONSE      <- "Zn"          # column holding the raw trace-element value (ppm)
RESPONSE_LOG  <- "logZn"       # name given to the log-transformed response
LOG_TRANSFORM <- TRUE          # paper Sect. 3.2, step 1

# Every covariate that is a candidate for characterising the geographical
# configuration. Step 20 whittles this list down by correlation and VIF.
CANDIDATES <- c("Elevation", "Slope", "Aspect", "Water",
                "NDVI", "SOC", "pH", "Road", "Mine")

# The set the geosimilarity vignette uses, kept here purely so step 20 can
# report whether our data-driven selection agrees with it.
VIGNETTE_VARS <- c("Slope", "Water", "NDVI", "SOC", "pH", "Road", "Mine")

# -- 3. Outlier screening -----------------------------------------------------
# removeoutlier() flags points beyond coef x IQR of the *log-transformed*
# response. The paper keeps high trace-element values on purpose (they mark
# mineral deposits); coef = 2.5 is the loose screen the package documents.
OUTLIER_COEF <- 2.5


# -- 4. Variable selection (paper Sect. 2.2.1) --------------------------------
COR_ALPHA     <- 0.05   # keep covariates significantly correlated with the response
VIF_THRESHOLD <- 4      # the paper's conservative multicollinearity threshold


# -- 5. Optimal similarity threshold (paper Eqs. 7-9) -------------------------
# kappa is the share of observations retained at each prediction location.
# The grid is fine below 0.1 because that is where the RMSE minimum sits, and
# coarse above it because the curve is nearly flat there.
KAPPA_GRID     <- c(seq(0.01, 0.10, 0.01), seq(0.2, 1.0, 0.1))
BESTKAPPA_NREPEAT <- 10   # cross-validation repeats inside gos_bestkappa()
BESTKAPPA_NSPLIT  <- 0.5  # training share of each split


# -- 6. Model comparison (paper Sect. 3.2, Table 3) ---------------------------
CV_REPEATS <- 50    # the paper's 50 repeated 50/50 splits
CV_SPLIT   <- 0.5   # training share

# The paper also benchmarks against ordinary and regression kriging; this
# tutorial compares the three models that do not need a variogram, so the
# whole pipeline stays inside geosimilarity and base R.
CV_MODELS  <- c("MLR", "BCS", "GOS")


# -- 7. Compute ---------------------------------------------------------------
# geosimilarity can fork, but the tutorial is meant to be reproducible on one
# core (and the companion Shinylive app has no multiprocessing at all).
CORES <- 1
SEED  <- 42


# -- 8. Figure style ----------------------------------------------------------
# Base R graphics only. Every figure is written as a PNG and a matching vector
# PDF; Greek letters go through plotmath so the PDF keeps them as text.
FIG_DPI      <- 196
FIG_DEVICES  <- c("png", "pdf")
FIG_W_SINGLE <- 9      # cm
FIG_W_DOUBLE <- 19     # cm

# Sequential ramps, both reversed so that dark = high. Base R's hcl.colors()
# ships "Rocket" (the magma family) and "Mako"; there is no "Magma" entry.
PAL_PRED  <- "Rocket"
PAL_UNCER <- "Mako"
PAL_N     <- 128

ACCENT      <- "#b03a2e"   # GOS brick red, matching the tutorial page
ACCENT_SOFT <- "#fbeeec"
PAL_MODEL   <- c(MLR = "#117864", BCS = "#6c3483", GOS = "#b03a2e")
