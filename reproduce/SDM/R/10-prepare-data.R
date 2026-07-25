# =============================================================================
# 10-prepare-data.R — read the analysis table and the geometries
#
# Outputs: results/variable-summary.csv   (what is in the modelling table)
#          results/walking-scales.csv      (the spatial scales of the model)
#          data/derived/blocks-joined.gpkg (geometry + delta, for the maps)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("sf", "reading the geometries")

log_head("Step 1/5  Prepare data")

d <- read_analysis()
log_info("analysis table: %d residential blocks, %d columns", nrow(d), ncol(d))
log_info("regions: %d (%s ...)", length(unique(d[[REGION_COL]])),
         paste(head(sort(unique(d[[REGION_COL]])), 3), collapse = ", "))

## -- the spatial scales the model is evaluated at -----------------------------
scales <- data.frame(walk_minutes = WALK_TIMES, distance_m = WALK_DISTANCES)
scales$access_col <- vapply(WALK_TIMES, col_access, character(1))
scales$accessibility_col <- vapply(WALK_TIMES, col_accessibility, character(1))
scales$delta_col <- vapply(WALK_TIMES, col_delta, character(1))
write_result(scales, F_SCALES)
log_info("scales: %s minutes (%s m) at %g km/h",
         paste(WALK_TIMES, collapse = "/"), paste(WALK_DISTANCES, collapse = "/"),
         WALK_SPEED_KMH)

## -- delta really is accessibility minus access -------------------------------
# The model's defining identity, checked rather than assumed.
worst <- 0
for (t in WALK_TIMES) {
  dev <- max(abs(d[[col_delta(t)]] - (d[[col_accessibility(t)]] - d[[col_access(t)]])))
  worst <- max(worst, dev)
}
log_info("delta = accessibility - access holds to %.1e across all scales", worst)
if (worst > 1e-4) log_warn("delta does not match beta - alpha; check the input table")

## -- what is in the table -----------------------------------------------------
vars <- c(vapply(WALK_TIMES, col_access, character(1)),
          vapply(WALK_TIMES, col_accessibility, character(1)),
          vapply(WALK_TIMES, col_delta, character(1)),
          VARS_RAW, VARS_CTX)
role <- c(rep("access (alpha)", length(WALK_TIMES)),
          rep("accessibility (beta)", length(WALK_TIMES)),
          rep("difference (delta)", length(WALK_TIMES)),
          rep("explanatory", length(VARS_RAW)),
          rep("explanatory, contextualised", length(VARS_CTX)))
summ <- data.frame(variable = vars, role = role,
  mean = round(vapply(vars, function(v) mean(d[[v]], na.rm = TRUE), numeric(1)), 4),
  sd   = round(vapply(vars, function(v) stats::sd(d[[v]], na.rm = TRUE), numeric(1)), 4),
  min  = round(vapply(vars, function(v) min(d[[v]], na.rm = TRUE), numeric(1)), 4),
  max  = round(vapply(vars, function(v) max(d[[v]], na.rm = TRUE), numeric(1)), 4),
  row.names = NULL)
write_result(summ, F_SUMMARY)

## -- geometry, joined to delta for the maps -----------------------------------
blocks <- sf::read_sf(file.path(PROJ_ROOT, BLOCKS_FILE))
blocks[[ID_COL]] <- as.character(blocks[[ID_COL]])
res <- blocks[blocks$MB_CATEGOR == CONSUMER_CAT, ]
log_info("geometry: %d blocks (%d %s, %d %s)", nrow(blocks),
         sum(blocks$MB_CATEGOR == CONSUMER_CAT), CONSUMER_CAT,
         sum(blocks$MB_CATEGOR == PROVIDER_CAT), PROVIDER_CAT)

keep <- c(ID_COL, REGION_COL,
          vapply(WALK_TIMES, col_access, character(1)),
          vapply(WALK_TIMES, col_accessibility, character(1)),
          vapply(WALK_TIMES, col_delta, character(1)))
joined <- merge(res[, ID_COL], d[, keep], by = ID_COL)
log_info("joined %d of %d analysis rows to geometry", nrow(joined), nrow(d))
sf::st_write(joined, file.path(DERIVED, "blocks-joined.gpkg"),
             delete_dsn = TRUE, quiet = TRUE)
log_info("wrote data/derived/blocks-joined.gpkg")
