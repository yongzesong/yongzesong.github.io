# =============================================================================
# 10-prepare-data.R — read the study layer, build the spatial weights
#
# Outputs: data/derived/analysis-layer.gpkg  (the working copy)
#          results/variable-summary.csv
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Step 1/5  Prepare data")

x <- read_layer()
preds <- predictor_names(x)
if (!RESPONSE %in% names(x)) stop("response '", RESPONSE, "' not in the layer")

log_info("%d spatial units, geometry: %s", nrow(x),
         as.character(sf::st_geometry_type(x)[1]))
log_info("CRS: %s", sf::st_crs(x)$input)
log_info("response: %s | %d explanatory variables: %s",
         RESPONSE, length(preds), paste(preds, collapse = ", "))

d <- sf::st_drop_geometry(x)
summ <- data.frame(
  variable = c(RESPONSE, preds),
  role = c("response", rep("explanatory", length(preds))),
  mean = round(vapply(c(RESPONSE, preds), function(v) mean(d[[v]], na.rm = TRUE), numeric(1)), 4),
  sd   = round(vapply(c(RESPONSE, preds), function(v) stats::sd(d[[v]], na.rm = TRUE), numeric(1)), 4),
  min  = round(vapply(c(RESPONSE, preds), function(v) min(d[[v]], na.rm = TRUE), numeric(1)), 4),
  max  = round(vapply(c(RESPONSE, preds), function(v) max(d[[v]], na.rm = TRUE), numeric(1)), 4),
  row.names = NULL)
write_result(summ, F_VARS)

# The weight matrix defines every neighbourhood used from here on.
wt <- build_weights(x)
nb_counts <- rowSums(wt > 0)
log_info("neighbours per unit: min %d, median %.0f, max %d",
         min(nb_counts), stats::median(nb_counts), max(nb_counts))
if (any(nb_counts == 0))
  log_warn("%d unit(s) have no neighbour; their geocomplexity is undefined",
           sum(nb_counts == 0))

sf::st_write(x, file.path(DERIVED, "analysis-layer.gpkg"), delete_dsn = TRUE, quiet = TRUE)
saveRDS(wt, file.path(DERIVED, "weights.rds"))
log_info("wrote data/derived/analysis-layer.gpkg and weights.rds")
