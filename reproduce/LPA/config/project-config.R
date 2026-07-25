# =============================================================================
# project-config.R — the ONLY file you edit when porting this template
#                    to a new study area or a new set of pathways.
# =============================================================================
# Everything below is read by R/00-config.R and used by every pipeline step.
# After editing this file, run:  Rscript run-all.R
# =============================================================================


# -- 1. Project identity ------------------------------------------------------
PROJECT_ID     <- "lpa-demo"
PROJECT_TITLE  <- "Local pathways of association among vegetation, climate and soil"
DOMAIN         <- "Tibetan Plateau ecosystem (NDVI, LST, precipitation, soil water)"
TARGET_JOURNAL <- "International Journal of Applied Earth Observation and Geoinformation"


# -- 2. Input data ------------------------------------------------------------
# One point table holding the coordinates and the observed variables. Every
# variable is already standardised; LPA reads the geometry as data, not as
# decoration, because the neighbourhood is what makes a coefficient "local".
#
# The same file also carries the published LPA output (seven lambda columns and
# their p-values), which step 40 uses as the reproduction target.

INPUT_FILE <- "data/lpa-tibetan-plateau.csv"
COORD_X    <- "x"          # longitude, decimal degrees
COORD_Y    <- "y"          # latitude, decimal degrees


# -- 3. Projecting degrees to kilometres --------------------------------------
# Ripley's K needs a metric plane. The published range (707.29 km) is recovered
# with an equirectangular projection scaled at the mean latitude of the points.
#   "equirect" — cos(lat) scaling, the convention that reproduces the paper
#   "degrees"  — leave coordinates in degrees (radii are then in degrees)
PROJECTION   <- "equirect"
KM_PER_DEG_Y <- 110.57     # metres per degree of latitude, mid-latitude value
KM_PER_DEG_X <- 111.32     # equatorial value, multiplied by cos(mean latitude)


# -- 4. The optimal local range -----------------------------------------------
# Step 20 estimates it from the data with Ripley's K and Besag's L. Edge
# correction matters more than anything else here: with no correction the L
# curve peaks at roughly half the published distance.
#   "border"    — reduced-sample correction, reproduces the published value
#   "isotropic" — Ripley's correction
#   "none"      — naive estimator, kept for the sensitivity comparison
K_CORRECTION  <- "border"
K_WINDOW      <- "rectangle"      # "rectangle" (bounding box) or "hull"
K_R_MAX       <- 1200             # km, upper end of the search grid
K_R_STEP      <- 0.01             # km, resolution of the search grid
K_CORRECTIONS_COMPARED <- c("none", "border", "isotropic")

# Set to a number to override the estimate and force a radius (km).
RADIUS_OVERRIDE <- NULL

# Radii used by the sensitivity check in step 50.
RADIUS_SENSITIVITY <- c(400, 500, 600, 705.29, 800, 900)

# A local fit needs enough neighbours to be worth trusting.
MIN_LOCAL_N <- 60


# -- 5. The structural equation model -----------------------------------------
# Fig. 5 of the paper draws four measurement arrows and three structural ones,
# but a latent variable measured by a single indicator is not identified: its
# loading is fixed at 1 and cannot take the negative or fractional values the
# published output contains. The reconstruction below gives every latent the
# two or three indicators the data file actually carries (EVI, T and PH are the
# reference indicators), which reproduces the sign and range of every path.
#
# Replace this string if you know the authors' original specification.

SEM_MODEL <- '
  Plant   =~ EVI + NDVI
  Climate =~ T + LST + P
  Soil    =~ PH + watercontent

  Soil  ~ Climate
  Plant ~ Climate + Soil
'

SEM_VARS <- c("EVI", "NDVI", "LST", "P", "T", "PH", "watercontent")

# The seven reported pathways, in the order they appear as lambda_1..lambda_7
# in Fig. 5 and as columns in the published output file.
#   label     — how the path is written in Table 2
#   lhs/op/rhs— how lavaan names the same parameter
#   published — the column holding the authors' estimate
PATHS <- data.frame(
  lambda    = paste0("lambda", 1:7),
  label     = c("Precipitation =~ Climate", "LST =~ Climate",
                "Water content =~ Soil", "NDVI =~ Plant",
                "Soil ~ Plant", "Climate ~ Plant", "Climate ~ Soil"),
  short     = c("P -> Climate", "LST -> Climate", "Water -> Soil",
                "NDVI -> Plant", "Soil -> Plant", "Climate -> Plant",
                "Climate -> Soil"),
  lhs       = c("Climate", "Climate", "Soil", "Plant", "Plant", "Plant", "Soil"),
  op        = c("=~", "=~", "=~", "=~", "~", "~", "~"),
  rhs       = c("P", "LST", "watercontent", "NDVI", "Soil", "Climate", "Climate"),
  published = c("P_climate", "LST_climate", "water_soil", "NDVI_plant",
                "soil_plant", "climate_plant", "climate_soil"),
  kind      = c(rep("Measurement", 4), rep("Structural", 3)),
  stringsAsFactors = FALSE
)

# lavaan reports a standardised solution; the published values behave the same
# way, including values outside [-1, 1] where factors are strongly correlated.
SEM_SOLUTION <- "standardized"    # "standardized" or "unstandardized"

# Locations to fit. NULL = every location in the file.
N_LOCATIONS <- NULL


# -- 6. Reporting conventions -------------------------------------------------
# Table 2 of the paper summarises lambda only where it falls inside [-1, 1] —
# the same filter the authors' plotting script applies — while the significance
# share is taken over the locations that returned a complete set of p-values.
LAMBDA_PLOT_RANGE <- c(-1, 1)
SIG_LEVEL         <- 0.05

# The class breaks printed in the legend of Fig. 5.
LAMBDA_BREAKS <- c(-1.0000, -0.7421, -0.0259, 0.6523, 0.9815, 1.0000)


# -- 7. Figure style ----------------------------------------------------------
FIG_WIDTH_SINGLE <- 9
FIG_WIDTH_DOUBLE <- 19
FIG_DPI          <- 600
FIG_DEVICES      <- c("pdf", "png")

# The five-class diverging palette of Fig. 5, blue (negative) to red (positive).
PALETTE_LAMBDA <- c("#4A7DB5", "#63BFB0", "#BFBFBF", "#F2A03D", "#E03C31")
PALETTE_SIG    <- c("<0.01" = "#1B7837", "<0.05" = "#A6DBA0", "Not sig" = "#BFBFBF")

SEED <- 42
