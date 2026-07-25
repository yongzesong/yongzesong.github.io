# =============================================================================
# 03-geocomplexity.R — geocomplexity (GC) pattern of every explanatory variable
#
# GC (Zhang et al. 2023, IJGIS) measures local spatial complexity: high values
# mark locations whose local pattern contradicts spatial dependence. LPI treats
# each variable's GC pattern as an additional explanatory variable, so this
# step doubles the variable set from {X} to {X, GC(X)}.
#
# Implementation: geocomplexity::geocd_vector(method = "spvar") with KNN
# spatial weights, k = max(knn_min, knn_share * n)   (cc005 Step 2).
#
# Output: results/step03-geocomplexity.csv
# =============================================================================

source("R/functions/fn-config.R")

library(sf)
library(sdsfun)
library(geocomplexity)

cfg <- load_config()
ensure_dir("results")

d      <- read_analysis_table(cfg)
x_vars <- cfg$predictors$x_vars

sf_data <- st_as_sf(d, coords = c(cfg$data$x_col, cfg$data$y_col),
                    crs = cfg$project$crs_input)

k_local <- max(cfg$params$gc$knn_min,
               round(nrow(d) * cfg$params$gc$knn_share))
cat(sprintf("Local neighbourhood size k = %d (%.0f%% of n = %d)\n",
            k_local, cfg$params$gc$knn_share * 100, nrow(d)))

wt <- sdsfun::spdep_contiguity_swm(sf_data, k = k_local)

cat("Computing geocomplexity for", length(x_vars), "variables...\n")
system.time({
  gc_result <- geocd_vector(sf_data[, x_vars], wt,
                            method    = cfg$params$gc$method,
                            normalize = TRUE)
})

gc_df <- as.data.frame(gc_result)
gc_df <- gc_df[, !grepl("^geometry$", names(gc_df)), drop = FALSE]
colnames(gc_df) <- gc_names_of(cfg)

out <- cbind(
  d[, c(cfg$data$id_col, cfg$data$x_col, cfg$data$y_col)],
  gc_df
)
write.csv(out, "results/step03-geocomplexity.csv", row.names = FALSE)
cat("Saved: results/step03-geocomplexity.csv\n")
