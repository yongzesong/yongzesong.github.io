# =============================================================================
# project-config.R — the ONLY file you edit when porting this template
#                    to a new domain.
# =============================================================================
# Everything below is read by R/00-config.R and used by every pipeline step.
# After editing this file, run:  Rscript run-all.R
# =============================================================================


# -- 1. Project identity ------------------------------------------------------
PROJECT_ID     <- "stor-demo"
PROJECT_TITLE  <- "Spatial trade-off relation between [QUANTITY] and [QUALITY] in [YOUR STUDY AREA]"
DOMAIN         <- "sustainable road infrastructure (simulated blocks)"
TARGET_JOURNAL <- "TBD"


# -- 2. Input data ------------------------------------------------------------
# The method needs ONE table: spatial units (blocks) with indicator variables.
# Each indicator belongs to a category, each category to a dimension (quantity
# A or quality B), and each carries a sign: +1 if more of it means more
# sustainable, -1 otherwise (Table 1 of the paper).

BLOCK_FILE <- "data/blocks.csv"

XCOL <- "x_km"                 # block centroid coordinates, km
YCOL <- "y_km"

# name = column in BLOCK_FILE; category groups variables for entropy weighting;
# dimension assigns the category to quantity (A) or quality (B).
INDICATORS <- data.frame(
  name = c("road_density", "main_road_density", "roads_per_person",
           "intersection_density",
           "roughness", "rutting", "deflection", "curvature",
           "crash_rate",
           "craf_schools", "craf_hospitals", "craf_industry",
           "dist_ports", "dist_airports"),
  category = c("density", "density", "density",
               "connectivity",
               "performance", "performance", "performance", "performance",
               "safety",
               "accessibility", "accessibility", "accessibility",
               "transport_modes", "transport_modes"),
  dimension = c("A", "A", "A", "A",
                "B", "B", "B", "B", "B",
                "B", "B", "B", "B", "B"),
  sign = c(+1, +1, +1, +1,
           -1, -1, -1, -1,
           -1,
           +1, +1, +1,
           -1, -1),
  stringsAsFactors = FALSE
)


# -- 3. Simulation ------------------------------------------------------------
# Only used by R/10-simulate-data.R. Replace that step with your own data and
# nothing here matters.
GRID_NX <- 40                  # blocks west-east
GRID_NY <- 40                  # blocks south-north
CELL_KM <- 10                  # block size, km

# Ground-truth utility curve embedded in the simulation. The logistic rise
# puts the maximum marginal utility at TRUE_T1; the slow linear gain keeps the
# curve climbing through the MR stage; the penalty beyond TRUE_T2 turns it
# down, so eta = 0 near TRUE_T2. The published case found t1 = 0.072 and
# t2 = 0.585 (Fig. 6b of Song et al. 2021).
TRUE_T1   <- 0.07              # IR -> MR boundary on Gamma_A
TRUE_T2   <- 0.585             # MR -> NR boundary on Gamma_A
U_BASE    <- 0.57              # utility at Gamma_A = 0
U_AMP     <- 0.20              # size of the logistic rise
U_SLOPE   <- 0.032             # logistic scale (smaller = steeper rise)
U_LIN     <- 0.10              # linear gain through the MR stage
U_DECLINE <- 0.80              # decline rate past TRUE_T2 (power 1.5)


# -- 4. Trade-off relation (LOESS + marginal utility) -------------------------
N_BINS     <- 40               # equal-count Gamma_A bins (display dots only)
LOESS_SPAN <- 0.15             # LOESS nearest-neighbour fraction, raw blocks
N_EVAL     <- 200              # evaluation points for u() and eta


# -- 5. Spatial clusters (LISA) -----------------------------------------------
LISA_NSIM  <- 999              # conditional permutations for pseudo p-values
LISA_ALPHA <- 0.05             # significance level for HH / LL clusters


# -- 6. Income contribution (GAM + GWR) ---------------------------------------
INCOME_COL   <- "income"       # response, 1000 $ per person, in BLOCK_FILE
GWR_NEIGHBORS <- c(40, 60, 80, 100, 150)   # adaptive bandwidth candidates
SEED <- 100


# -- 7. Figure style ----------------------------------------------------------
FIG_WIDTH_SINGLE <- 9
FIG_WIDTH_DOUBLE <- 19
FIG_DPI          <- 600
FIG_DEVICES      <- c("pdf", "png")

PALETTE_DMU  <- c(IR = "#74C7E8", MR = "#F2C14E", NR = "#F25C54")
PALETTE_DIM  <- c(Gamma_A = "#56B4E9", Gamma_B = "#F08A5D", Gamma = "#4CAF6E")
PALETTE_LISA <- c("Hotspot" = "#B2182B", "Cold spot" = "#2166AC",
                  "Others" = "#D9D9D9")
PALETTE_SEQ  <- c("#2C3E8C", "#3FA0C0", "#8CD07C", "#F2E34C", "#E8A03C",
                  "#C0503C")
