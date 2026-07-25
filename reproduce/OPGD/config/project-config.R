# =============================================================================
# project-config.R — the ONLY file you edit when porting this template
#                    to a new domain.
# =============================================================================
# Everything below is read by R/00-config.R and used by every pipeline step.
# After editing this file, run:  Rscript run-all.R
# =============================================================================


# -- 1. Project identity ------------------------------------------------------
PROJECT_ID     <- "opgd-demo"
PROJECT_TITLE  <- "Optimal parameters-based geographical detector for [YOUR RESPONSE] in [YOUR STUDY AREA]"
DOMAIN         <- "vegetation change (NDVI)"       # e.g. "PM2.5", "road damage"
TARGET_JOURNAL <- "TBD"


# -- 2. Input data ------------------------------------------------------------
# The pipeline needs ONE table: one row per spatial unit, one response column,
# a set of categorical explanatory variables and a set of continuous ones.
#
# DATASET names a built-in GD dataset (ndvi_5/10/20/30/40/50, h1n1_50/100/150)
# so the template runs with no external data. Set INPUT_FILE to a CSV path to
# use your own data instead (then DATASET is ignored).

DATASET     <- "ndvi_40"                           # a GD built-in table
INPUT_FILE  <- NULL                                # e.g. "data/raw/my-samples.csv"

RESPONSE    <- "NDVIchange"                         # the dependent variable
CATEGORICAL <- c("Climatezone", "Mining")          # already-discrete explanatory vars
CONTINUOUS  <- c("Tempchange", "Precipitation",    # continuous explanatory vars —
                 "GDP", "Popdensity")              #   these get optimal discretisation


# -- 3. Optimal discretisation parameters -------------------------------------
# For every continuous variable the OPGD model searches this grid of
# (method x number-of-intervals) and keeps the combination with the highest
# factor-detector Q value. Methods: equal, natural, quantile, geometric, sd.

DISC_METHODS   <- c("equal", "natural", "quantile", "geometric", "sd")
DISC_INTERVALS <- 3:7                              # break numbers to try


# -- 4. Spatial-scale optimisation (SESU) -------------------------------------
# OPGD also selects a spatial unit size. The template re-runs the model across
# a series of built-in datasets aggregated to different unit sizes and reports
# where the Q values stabilise.

SESU_DATASETS <- c("ndvi_20", "ndvi_30", "ndvi_40", "ndvi_50")
SESU_SIZES    <- c(20, 30, 40, 50)                 # unit sizes (km) matching above


# -- 5. Modules to run --------------------------------------------------------
RUN_FACTOR      <- TRUE     # factor detector      (relative importance, Q)
RUN_INTERACTION <- TRUE     # interaction detector (pairwise Q + 5 types)
RUN_RISK        <- TRUE     # risk detector        (sub-region means + t-test)
RUN_ECOLOGICAL  <- TRUE     # ecological detector  (F-test between variables)
RUN_SESU        <- TRUE     # spatial-scale effects


# -- 6. Figure style ----------------------------------------------------------
FIG_WIDTH_SINGLE <- 9                              # cm
FIG_WIDTH_DOUBLE <- 19                             # cm
FIG_DPI          <- 600
FIG_DEVICES      <- c("pdf", "png")

SEED <- 42
