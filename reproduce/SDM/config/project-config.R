# =============================================================================
# project-config.R — the ONLY file you edit when porting this template
#                    to a new domain.
# =============================================================================
# Everything below is read by R/00-config.R and used by every pipeline step.
# After editing this file, run:  Rscript run-all.R
# =============================================================================


# -- 1. Project identity ------------------------------------------------------
PROJECT_ID     <- "sdm-demo"
PROJECT_TITLE  <- "Spatial Delta Model for [YOUR SERVICE] accessibility in [YOUR CITY]"
DOMAIN         <- "urban greenspace, Perth"
TARGET_JOURNAL <- "TBD"


# -- 2. Input data ------------------------------------------------------------
# ANALYSIS_FILE is the modelling table: one row per residential block, holding
# access (u), accessibility (v) and their difference (df) at every walking time,
# plus the explanatory variables. It is the authors' own table, so the results
# below are the published ones.
#
# BLOCKS_FILE and REGIONS_FILE are the geometries: used for mapping, and for the
# demonstration in Step 2 that recomputes accessibility from scratch.

ANALYSIS_FILE <- "data/mbvars.csv"
BLOCKS_FILE   <- "data/blocks.gpkg"
REGIONS_FILE  <- "data/regions.gpkg"

ID_COL     <- "MB_CODE21"      # joins the table to the geometry
REGION_COL <- "SA3"            # region label in the analysis table


# -- 3. Spatial scales --------------------------------------------------------
# The model is evaluated at a series of walking times. Distance follows from an
# average pedestrian speed of 5 km/h (paper Table 2).
WALK_TIMES     <- c(3, 5, 10, 15, 20, 30)          # minutes
WALK_DISTANCES <- c(250, 417, 833, 1250, 1667, 2500)  # metres
WALK_SPEED_KMH <- 5

# Column prefixes in ANALYSIS_FILE: access, accessibility, and their difference.
ACCESS_PREFIX        <- "u"    # alpha
ACCESSIBILITY_PREFIX <- "v"    # beta
DELTA_PREFIX         <- "df"   # delta = beta - alpha


# -- 4. The accessibility demonstration (Step 2) ------------------------------
# Recomputes accessibility from the block geometry with the catchment package,
# to show how beta is produced. Set to FALSE to skip; the rest of the pipeline
# uses the authors' values either way.
#
# NOTE. The published beta used road-network distances built in QGIS (see the
# tutorial). This demonstration uses straight-line distances, so it reproduces
# the tutorial's own printed numbers, not the published beta.
RUN_ACCESS_DEMO <- TRUE
DEMO_TIME       <- 3           # which walking time to demonstrate (minutes)
CONSUMER_CAT    <- "Residential"
PROVIDER_CAT    <- "Parkland"
CONSUMER_VALUE  <- "Person"        # demand
PROVIDER_VALUE  <- "AREASQKM21"    # supply


# -- 5. Explanatory variables -------------------------------------------------
# Six geospatial variables, each also in a contextualised form that carries the
# local spatial pattern of the variable rather than its value alone.
VARS_RAW <- c("popdenskm2", "dwedenskm2", "shapefacto",
              "compactrat", "neargsdist", "neargsarea")
VARS_CTX <- c("lisav1", "lisav2", "lisav3", "lisav4", "lisav5", "lisav6")

VARS_LABEL <- c("Population density", "Dwelling density", "Shape factor",
                "Compact ratio", "Distance to near greenspace",
                "Area of near greenspace")


# -- 6. Driver analysis (GOZH) ------------------------------------------------
# Each variable is turned into spatial strata by a regression tree, then the
# geographical detector measures how much of delta those strata explain.
TREE_MINBUCKET <- 10           # smallest terminal node the tree may create
RUN_DRIVERS      <- TRUE
RUN_INTERACTIONS <- TRUE       # pairwise interaction detector
INTERACTION_TIME <- 5          # walking time for the interaction table


# -- 7. Figure style ----------------------------------------------------------
FIG_WIDTH_SINGLE <- 9
FIG_WIDTH_DOUBLE <- 19
FIG_DPI          <- 300   # the maps carry ~8,000 polygons; 600 dpi doubles
                          # the file size for no visible gain
FIG_DEVICES      <- c("pdf", "png")

PALETTE_SEQ   <- c("#FFF7EC", "#FDD49E", "#FC8D59", "#D7301F", "#7F0000")
PALETTE_DIV   <- c("#2166AC", "#92C5DE", "#F7F7F7", "#F4A582", "#B2182B")
PALETTE_DELTA <- c(negative = "#2166AC", zero = "#BBBBBB", positive = "#B2182B")

SEED <- 42
