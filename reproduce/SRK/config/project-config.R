# =============================================================================
# project-config.R — the ONLY file you edit when porting this pipeline
#                    to a new study area.
# =============================================================================
# Everything below is read by R/00-config.R and used by every pipeline step.
# After editing this file, run:  Rscript run-all.R
# =============================================================================


# -- 1. Project identity ------------------------------------------------------
PROJECT_ID     <- "srk-wa-trace-elements"
PROJECT_TITLE  <- "Singularity regression kriging for [YOUR RESPONSE] in [YOUR STUDY AREA]"
DOMAIN         <- "trace-element geochemistry, Western Australia"
TARGET_JOURNAL <- "TBD"


# -- 2. Input data ------------------------------------------------------------
# The method needs ONE table per response: point observations with coordinates
# in a projected CRS (metres) and one column per covariate. The case-study
# table is downloaded by R/20-case-data.R from the authors' repository; set
# DATA_SOURCE to "local" and drop your own CSVs into data/ to use your data.

DATA_SOURCE <- "download"            # "download" | "local"
DATA_BASE_URL <- paste0("https://raw.githubusercontent.com/renkaigis/",
                        "Singularity_Regression_Kriging/HEAD/raw%20data")

XCOL <- "x"                          # projected easting,  metres (EPSG:3857)
YCOL <- "y"                          # projected northing, metres

# One entry per response variable. `xvars` is the covariate set the paper's
# Spearman screen (p < 0.05) retained for that element; R/20-case-data.R
# re-runs the screen and reports whether it reproduces this set.
#
# The paper maps two trace elements; this tutorial follows Co end to end. Every
# step below is written as a loop over ELEMENTS, so adding the second element
# back is one entry — uncomment the Zn block and re-run:
#
#   Zn = list(file  = "dt_Zn_linear.csv",
#             yvar  = "Zn",
#             xvars = c("ss", "ifi", "hm", "elevation", "slope", "aspect"),
#             unit  = "ppm"),
ELEMENTS <- list(
  Co = list(file  = "dt_Co_linear.csv",
            yvar  = "Co",
            xvars = c("ifi", "hm", "elevation"),
            unit  = "ppm")
)

# Every candidate covariate offered to the Spearman screen in step 20.
CANDIDATE_XVARS <- c("ss", "ifi", "imi", "hm", "elevation", "slope", "aspect")


# -- 3. Singularity features (paper Eq. 1-4) ----------------------------------
# Scales are radii of the square scanning window, in the units of XCOL/YCOL.
# The paper uses 2-20 km at 2-km steps on Web-Mercator metres.
SV_SCALES        <- seq(2000, 20000, by = 2000)
MIN_PTS_PER_SCALE <- 3L              # a scale needs this many neighbours
MIN_VALID_SCALES  <- 2L              # an index needs this many valid scales
SV_NEUTRAL        <- 2.0             # value assigned when estimation fails
SV_SD_THRESHOLD   <- 0.5             # drop sv features flatter than this

# Simulation scales are in grid cells, not metres (paper Sec. 2.3).
SV_SCALES_SIM     <- seq(1, 10, by = 1)


# -- 4. Trend model and residual kriging --------------------------------------
RF_NTREE      <- 500L
RF_SEED       <- 42L                 # the authors' scripts leave this unset;
                                     # see R/50-seed-stability.R for why it matters
RF_USE_COORDS <- TRUE                # authors' case-study code feeds x, y to the
                                     # forest; the paper's Eq. 5 does not. Set
                                     # FALSE to follow the equation instead.
VARIOGRAM_FIT <- "automap"           # "automap" (as published) | "gstat-sph"


# -- 5. Validation ------------------------------------------------------------
CV_FOLDS      <- 5L
CV_BLOCK_SIZE <- 15000               # spatial block edge, metres
CV_SEED       <- 123L                # seeds the block-to-fold assignment
SEED_REPEATS  <- 20L                 # RF seeds used by R/50-seed-stability.R


# -- 6. Sensitivity analysis (paper Fig. 7) -----------------------------------
SENS_MAX_SCALES <- seq(10000, 20000, by = 2000)   # largest scale kept
SENS_THRESHOLDS <- c(0.3, 0.4, 0.5, 0.6, 0.7)     # sv SD cut-offs


# -- 7. Simulation experiment (paper Sec. 2.3) --------------------------------
SIM_SIDE     <- 20L                  # grid is SIM_SIDE x SIM_SIDE
SIM_RANGE_Y  <- 10                   # spherical range of the response field
SIM_RANGE_X  <- 8                    # spherical range of the noise field
SIM_MIX      <- 0.85                 # covariate = MIX * Y + (1 - MIX) * noise
SIM_SHIFT    <- 6                    # additive shift keeping fields positive
SIM_TRAIN    <- 0.7                  # training share
SEED         <- 42L


# -- 8. Figures ---------------------------------------------------------------
FIG_DPI          <- 196
FIG_WIDTH_SINGLE <- 9                # cm
FIG_WIDTH_DOUBLE <- 19               # cm
FIG_DEVICES      <- c("png", "pdf")
PAL_SEQ          <- "Rocket"         # hcl.colors palette for value maps
PAL_DIV          <- "Blue-Red 3"     # hcl.colors palette for signed maps
