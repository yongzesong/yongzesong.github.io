# =============================================================================
# project-config.R — the ONLY file you edit when porting this template
#                    to a new domain.
# =============================================================================
# Everything below is read by R/00-config.R and used by every pipeline step.
# After editing this file, run:  Rscript run-all.R
#
# Read plan/adaptation-guide.md before changing anything here.
# =============================================================================


# -- 1. Project identity ------------------------------------------------------
# Used in file headers, table captions and the manuscript metadata block.

PROJECT_ID     <- "dsi-demo"                       # short slug, hyphens only
PROJECT_TITLE  <- "Degree of spatial interpretability for [YOUR RESPONSE VARIABLE] prediction in [YOUR STUDY AREA]"
DOMAIN         <- "synthetic demonstration"        # e.g. "soil organic carbon", "urban heat", "PM2.5"
TARGET_JOURNAL <- "TBD"


# -- 2. Input data ------------------------------------------------------------
# The pipeline needs ONE table with these columns:
#   lon, lat  — coordinates in DATA_CRS
#   <response> — the dependent variable
#   <predictors...> — everything used for modelling and for Q-value stratification
#
# Set INPUT_FILE to NULL to run on the built-in synthetic dataset
# (R/09-make-synthetic-data.R). Use that first to check the pipeline runs,
# then point INPUT_FILE at your own CSV.

INPUT_FILE  <- NULL                                # e.g. "data/raw/my-samples.csv"
RESPONSE    <- "y"                                 # name of the dependent variable column
PREDICTORS  <- NULL                                # NULL = every column except lon/lat/response
DROP_COLS   <- character(0)                        # columns to exclude (IDs, dates, duplicates)

LOG_TRANSFORM_RESPONSE <- FALSE                    # TRUE for right-skewed responses (biomass, income)


# -- 3. Coordinate reference systems ------------------------------------------
# DATA_CRS       — CRS the lon/lat columns are stored in (4326 = WGS84).
# PROJECTED_CRS  — MUST be a projected CRS in metres. Neighbour distances are
#                  computed in this CRS; using degrees makes Moran's I wrong.
#   Australia 3577 | contiguous US 5070 | Europe 3035 | China 4479 |
#   anywhere else: the local UTM zone.

DATA_CRS      <- 4326
PROJECTED_CRS <- 3577


# -- 4. Spatial metric settings -----------------------------------------------
# K_MORAN  — k for the k-nearest-neighbour weights used by Moran's I.
#            6-10 for dense samples, 10-20 for sparse samples.
# K_GEOC   — k for the neighbourhood used by geocomplexity (DG module).
#            The DG paper used 18. Must be large enough that neighbours share
#            neighbours, which is what local complexity is built from.
# K_SENSITIVITY — values of k re-run in step 50 to show results are not an
#            artefact of one neighbourhood size.

K_MORAN       <- 8
K_GEOC        <- 18
K_SENSITIVITY <- c(6, 8, 10, 14, 18, 22)

MORAN_ALTERNATIVE <- "greater"                     # "greater", "less", "two.sided"

# Stratification for the Q value (spatial heterogeneity). "tree" reproduces the
# DSI paper: a regression tree on the predictors defines the strata, and every
# model's residuals are scored against those same strata. "kmeans" clusters the
# standardised predictors instead. Both partition space by the explanatory
# variables; strata cut from the response would make the Q value of the
# response near 1 by construction.
STRATIFY_METHOD <- "tree"                          # "tree" or "kmeans"
STRATIFY_K      <- 12                              # target strata count for "kmeans"


# -- 5. Models to evaluate ----------------------------------------------------
# Names must exist in the registry in R/05-models.R. Models whose package is
# not installed are skipped with a message rather than failing the run, so the
# pipeline always produces output. See env/requirements.md to install the rest.

MODELS <- c("rpart", "rf", "xgbTree", "cubist", "knn",
            "pls", "earth", "svmRadial", "gbm")

TRAIN_FRACTION <- 0.7                              # 70/30 split, as in both papers
SEED           <- 42


# -- 6. Modules to run --------------------------------------------------------
# DSI (global: Moran's I + Q value) is the core. DG (local: geocomplexity) is
# the spatially explicit extension and needs the geocomplexity package.

RUN_DSI         <- TRUE
RUN_DG          <- TRUE
RUN_SENSITIVITY <- TRUE


# -- 7. Figure style ----------------------------------------------------------

FIG_WIDTH_SINGLE <- 9                              # cm
FIG_WIDTH_DOUBLE <- 19                             # cm
FIG_DPI          <- 600
FIG_DEVICES      <- c("pdf", "png")

PALETTE_MODELS <- c(
  lm        = "#4E79A7", rpart  = "#A0CBE8", rf      = "#F28E2B",
  xgbTree   = "#FFBE7D", gam    = "#59A14F", cubist  = "#8CD17D",
  knn       = "#B6992D", pls    = "#499894", earth   = "#86BCB6",
  svmRadial = "#E15759", gbm    = "#FF9D9A"
)
PALETTE_SEQ <- c("#FFF7EC", "#FDD49E", "#FC8D59", "#D7301F", "#7F0000")
PALETTE_DIV <- c("#2166AC", "#92C5DE", "#F7F7F7", "#F4A582", "#B2182B")
