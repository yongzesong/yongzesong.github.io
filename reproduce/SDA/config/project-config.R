# =============================================================================
# project-config.R — the ONLY file you edit when porting this template
#                    to a new domain.
# =============================================================================
# Everything below is read by R/00-config.R and used by every pipeline step.
# After editing this file, run:  Rscript run-all.R
# =============================================================================


# -- 1. Project identity ------------------------------------------------------
PROJECT_ID     <- "sda-demo"
PROJECT_TITLE  <- "Second dimension of spatial association for [YOUR RESPONSE] in [YOUR STUDY AREA]"
DOMAIN         <- "geochemical mapping (trace elements)"
TARGET_JOURNAL <- "TBD"


# -- 2. Input data ------------------------------------------------------------
# The method needs TWO tables:
#   POINT_FILE — sample locations with the response  (lon, lat, response)
#   GRID_FILE  — a dense grid covering the study area (lon, lat, surface(s))
# The second dimension is built by summarising the GRID around each POINT, so
# the grid is what carries the information "outside" the sample locations.

POINT_FILE <- "data/obs.csv"
GRID_FILE  <- "data/grids.csv"

LON <- "Lon"
LAT <- "Lat"

RESPONSE     <- "Cr_ppm"       # the dependent variable, in POINT_FILE
LOG_RESPONSE <- TRUE           # log-transform a right-skewed response
OUTLIER_SD   <- 2.5            # drop training values beyond this many SD


# -- 3. Second-dimension parameters -------------------------------------------
# For every grid surface, one variable is generated per (buffer, quantile)
# pair, so the count is length(DIST_BUFFERS) x length(QUANTILES).
#   b   — searching range in km: how far outside the sample to look
#   tau — quantile probability: which part of the local distribution to use

DIST_BUFFERS <- c(1, 3, 5, 7, 9)          # b, km
QUANTILES    <- seq(0, 1, 0.1)            # tau

# Grid surfaces to turn into second-dimension variables. The demo grid file
# ships Elevation only; the remaining seven surfaces of the published case are
# supplied as pre-generated variable sets (see REFERENCE_VARS below).
GRID_SURFACES <- c("Elevation")

# Pre-generated second-dimension variable sets, one CSV per surface, each with
# length(DIST_BUFFERS) x length(QUANTILES) columns and one row per sample.
REFERENCE_VARS <- c("Elevation", "Slope", "Aspect", "Water",
                    "NDVI", "pH", "SOC", "Road")
REFERENCE_DIR  <- "data/reference"

# First-dimension (FDA) variables: the same surfaces read at the sample points.
FDA_FILE <- "data/sample-vars-fda.csv"


# -- 4. Variable selection ----------------------------------------------------
# Variables are ranked by |correlation with the response|, then walked down that
# order dropping any that pushes the variance inflation factor above VIF_MAX.
VIF_MAX <- 10


# -- 5. Cross validation and models -------------------------------------------
TRAIN_FRACTION <- 0.7
SEED           <- 100

RUN_LM <- TRUE                 # linear model, as in the published vignette
RUN_RF <- TRUE                 # random forest, needs the randomForest package
RF_TREES <- 500


# -- 6. Figure style ----------------------------------------------------------
FIG_WIDTH_SINGLE <- 9
FIG_WIDTH_DOUBLE <- 19
FIG_DPI          <- 600
FIG_DEVICES      <- c("pdf", "png")

PALETTE_MODEL <- c(SDA = "#B2182B", FDA = "#2166AC")
PALETTE_SEQ   <- c("#FFF7EC", "#FDD49E", "#FC8D59", "#D7301F", "#7F0000")
