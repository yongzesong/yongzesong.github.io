# =============================================================================
# 06-local-interactions.R — local pairwise pattern interactions
#
# Pairs are formed among the top-N variables by mean local q (N from config;
# N=5 gives 10 pairs). For every location, each pair member is discretized
# inside the local window, single and combined-zone q values are computed
# (gdverse::gd), and the interaction is typed into the 5 geographical
# detector classes (cc005 Step 3e / LPI Fig 9).
#
# Outputs: results/step06-all-pairwise-interactions.csv
#          results/step06-local-best-interaction.csv
# =============================================================================

source("R/functions/fn-config.R")
source("R/functions/fn-lpi-core.R")

library(sf)
library(gdverse)
library(sdsfun)
library(dplyr)

cfg <- load_config()
ensure_dir("results")

d        <- build_model_frame(cfg)
all_vars <- all_vars_of(cfg)

spatial   <- build_spatial(d, cfg)
threshold <- read_local_range(cfg)$threshold_m

local_q <- read.csv("results/step05-local-q-values.csv")

# --- Top-N variables by mean local q ----------------------------------------
top_n  <- cfg$params$interaction$top_n_vars
mean_q <- sort(colMeans(local_q[, all_vars], na.rm = TRUE), decreasing = TRUE)
top_vars <- names(mean_q)[1:top_n]

cat(sprintf("Top %d variables by mean local q:\n", top_n))
for (v in top_vars) cat(sprintf("  %-24s %.4f\n", v, mean_q[v]))

var_pairs <- combn(top_vars, 2, simplify = FALSE)
cat(sprintf("\nRunning local interaction for %d pairs x %d locations...\n",
            length(var_pairs), nrow(d)))

system.time({
  inter_all <- run_interactions_all(d, cfg, var_pairs,
                                    spatial$distmat, threshold)
})

# --- Best interaction per location -------------------------------------------
coord_lookup <- data.frame(
  LocationID = seq_len(nrow(d)),
  Longitude  = d[[cfg$data$x_col]],
  Latitude   = d[[cfg$data$y_col]]
)

inter_best <- inter_all %>%
  group_by(LocationID) %>%
  slice_max(qv12, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(coord_lookup, by = "LocationID") %>%
  select(LocationID, Longitude, Latitude,
         var1, var2, qv1, qv2, qv12, interaction)

write.csv(inter_all,  "results/step06-all-pairwise-interactions.csv",
          row.names = FALSE)
write.csv(inter_best, "results/step06-local-best-interaction.csv",
          row.names = FALSE)
cat("Saved: step06-all-pairwise-interactions.csv, step06-local-best-interaction.csv\n")

# Console summary: strongest pairs by mean qv12.
pair_summary <- inter_all %>%
  group_by(var1, var2) %>%
  summarise(mean_qv12 = mean(qv12, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_qv12))
cat("\nPairs by mean interaction q (qv12):\n")
print(as.data.frame(head(pair_summary, 10)))
