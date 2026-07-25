# =============================================================================
# project-config.R — the ONLY file you edit when porting this template
#                    to a new domain.
# =============================================================================
# Everything below is read by R/00-config.R and used by every pipeline step.
# After editing this file, run:  Rscript run-all.R
# =============================================================================


# -- 1. Project identity ------------------------------------------------------
PROJECT_ID     <- "gc-demo"
PROJECT_TITLE  <- "Geocomplexity of [YOUR RESPONSE] in [YOUR STUDY AREA]"
DOMAIN         <- "economic inequality (Gini coefficient)"
TARGET_JOURNAL <- "TBD"


# -- 2. Input data ------------------------------------------------------------
# One polygon (or point) layer holding the response and the explanatory
# variables. Geocomplexity is computed from the spatial arrangement of those
# variables, so the geometry is part of the data, not decoration.

INPUT_FILE <- "data/econineq.gpkg"
RESPONSE   <- "Gini"

# NULL = every remaining column is an explanatory variable.
PREDICTORS <- NULL


# -- 3. Spatial weights -------------------------------------------------------
# Geocomplexity asks how a variable behaves in a neighbourhood, so the
# neighbourhood definition is a modelling choice, not a detail.
#   contiguity — polygons sharing a boundary (queen or rook)
#   knn        — k nearest neighbours, for points or irregular polygons
WEIGHTS_TYPE  <- "contiguity"     # "contiguity" or "knn"
WEIGHTS_QUEEN <- TRUE             # contiguity: queen (TRUE) or rook (FALSE)
WEIGHTS_STYLE <- "B"              # "B" binary, "W" row-standardised
WEIGHTS_K     <- 8                # knn only


# -- 4. Geocomplexity ---------------------------------------------------------
# geocd_* measures the complexity of each variable one at a time;
# geocs_* measures the complexity of the whole geographical configuration.
#   moran   — local Moran decomposition   (package default)
#   spvar   — spatial variance / fluctuation
#   shannon — Shannon entropy of the neighbourhood
GC_METHOD  <- "moran"
GC_METHODS_COMPARED <- c("moran", "spvar", "shannon")
GC_NORMALIZE <- TRUE


# -- 5. Models whose errors are explained -------------------------------------
# The paper's argument: fit conventional models, then ask geocomplexity to
# explain what they got wrong.
RUN_MLR <- TRUE
RUN_SVR <- TRUE                   # needs e1071
RUN_GWR <- TRUE                   # needs GWmodel

SVR_COST  <- 1.4                  # the paper's cross-validated optimum
SVR_GAMMA <- 0.2
GWR_KERNEL   <- "gaussian"
GWR_ADAPTIVE <- TRUE
GWR_APPROACH <- "AIC"             # bandwidth criterion: "AIC" or "CV"

# Which geocomplexity variables explain the errors. The paper selects the two
# with the clearest association; NULL uses every one.
ERROR_EXPLAIN_VARS <- c("Income", "Indemp")

# -- 6. Model improvement -----------------------------------------------------
# The practical consequence: if geocomplexity explains the errors, adding it
# should improve the model.
RUN_GCMLR   <- TRUE               # geocomplexity as extra explanatory variables
RUN_GEOCGWR <- TRUE               # geocomplexity-weighted GWR (gwr_geoc)


# -- 7. Figure style ----------------------------------------------------------
FIG_WIDTH_SINGLE <- 9
FIG_WIDTH_DOUBLE <- 19
FIG_DPI          <- 600
FIG_DEVICES      <- c("pdf", "png")

PALETTE_SEQ   <- c("#FFF7EC", "#FDD49E", "#FC8D59", "#D7301F", "#7F0000")
PALETTE_MODEL <- c(MLR = "#4E79A7", SVR = "#F28E2B", GWR = "#59A14F")

SEED <- 42
