# =============================================================================
# 20-access-accessibility.R — how access and accessibility are produced
#
# Access (alpha) is the greenspace area reachable within the walking distance:
# a count of supply, indifferent to how the supply is arranged.
#
# Accessibility (beta) is a Modified Gaussian Two-Step Floating Catchment Area
# (MG2SFCA): supply is discounted by distance and shared out among everyone who
# competes for it, so it carries crowding and travel effort as well as quantity.
#
# This step recomputes both from the block geometry so the reader can see where
# they come from. It is a demonstration: the published values used road-network
# distances built in QGIS, and these use straight-line distances, so they match
# the tutorial's printed figures rather than the paper's columns. Everything
# downstream uses the authors' values from the analysis table.
#
# Output: results/accessibility-demo.csv
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Step 2/5  How access and accessibility are computed")

if (!isTRUE(RUN_ACCESS_DEMO)) {
  log_info("RUN_ACCESS_DEMO is FALSE; skipping the demonstration")
} else if (!has_pkg("catchment")) {
  log_warn("the 'catchment' package is not installed; skipping the demonstration")
  log_warn("install with: remotes::install_github('uva-bi-sdad/catchment')")
} else {

need_pkg("sf", "geometry")
blocks <- sf::read_sf(file.path(PROJ_ROOT, BLOCKS_FILE))
cons <- blocks[blocks$MB_CATEGOR == CONSUMER_CAT, ]
prov <- blocks[blocks$MB_CATEGOR == PROVIDER_CAT, ]

d0 <- WALK_DISTANCES[match(DEMO_TIME, WALK_TIMES)]
log_info("demonstrating at %d minutes = %d m (%g km/h)", DEMO_TIME, d0, WALK_SPEED_KMH)
log_info("consumers: %d %s blocks | providers: %d %s blocks",
         nrow(cons), CONSUMER_CAT, nrow(prov), PROVIDER_CAT)

cc <- suppressWarnings(sf::st_centroid(cons))
pc <- suppressWarnings(sf::st_centroid(prov))

## -- access: supply reachable within the distance -----------------------------
D <- sf::st_distance(cc, pc)
units(D) <- NULL
alpha <- as.numeric((D <= d0) %*% prov[[PROVIDER_VALUE]])
reach <- mean(alpha > 0)
log_info("access: greenspace area within %d m; %.1f%% of blocks reach any greenspace",
         d0, 100 * reach)
if (reach < 0.5)
  log_info("at this distance most blocks reach none, so access is zero for them —")
if (reach < 0.5)
  log_info("a short walking time makes access a coarse, mostly-empty measure")
log_info("access among blocks that reach some: median %.4f km2",
         stats::median(alpha[alpha > 0]))

## -- accessibility: MG2SFCA, supply discounted by distance and shared ---------
beta <- catchment::catchment_ratio(
  consumers = cc, providers = pc, weight = "gaussian",
  consumers_value = CONSUMER_VALUE, providers_value = PROVIDER_VALUE,
  consumers_id = ID_COL, providers_id = ID_COL,
  max_cost = d0, return_type = "region", verbose = FALSE)
log_info("accessibility: MG2SFCA with a Gaussian decay, median %.6f", stats::median(beta))

demo <- data.frame(id = as.character(cons[[ID_COL]]),
                   access_demo = round(alpha, 6),
                   accessibility_demo = round(as.numeric(beta), 8))
names(demo)[1] <- ID_COL
write_result(demo, F_DEMO)

## -- the two measures answer different questions ------------------------------
# Compared only where access is defined at all: with a short walking time most
# blocks reach no greenspace, and a correlation over those zeros says nothing.
k <- demo$access_demo > 0
if (sum(k) > 30) {
  r <- stats::cor(demo$access_demo[k], demo$accessibility_demo[k])
  log_info("where greenspace is reachable, the two correlate at r = %.3f", r)
  log_info("same supply, different question: one counts it, the other discounts")
  log_info("it by distance and shares it among everyone competing for it")
}

## -- and how far this demonstration is from the published values --------------
d <- read_analysis()
m <- merge(demo, d[, c(ID_COL, col_access(DEMO_TIME), col_accessibility(DEMO_TIME))],
           by = ID_COL)
if (nrow(m) > 0) {
  ra <- stats::cor(m$access_demo, m[[col_access(DEMO_TIME)]])
  rb <- stats::cor(m$accessibility_demo, m[[col_accessibility(DEMO_TIME)]])
  log_info("against the published columns: access r = %.3f, accessibility r = %.3f", ra, rb)
  log_info("straight-line distances do not reproduce network-based accessibility;")
  log_info("the published values are used from Step 3 onward.")
}
}
