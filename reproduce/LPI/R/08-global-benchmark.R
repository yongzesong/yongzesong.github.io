# =============================================================================
# 08-global-benchmark.R — OPGD-style global q benchmark + validation table
#
# The LPI paper validates local results against the global OPGD power of
# determinant (LPI Table 4). This script computes global q for every variable
# and for the strongest interaction pairs, then assembles the validation
# table: local mean [min, max], % significant, and global q side by side
# (cc005 Steps 4c + 5).
#
# Outputs: results/step08-global-q.csv
#          results/step08-validation-table.csv
# =============================================================================

source("R/functions/fn-config.R")
source("R/functions/fn-lpi-core.R")

library(gdverse)
library(dplyr)

cfg <- load_config()
ensure_dir("results")

d        <- build_model_frame(cfg)
all_vars <- all_vars_of(cfg)
resp     <- cfg$response$response_name
n_strata <- cfg$params$global$n_strata

local_q   <- read.csv("results/step05-local-q-values.csv")
local_sig <- read.csv("results/step05-local-q-sig.csv")
inter_all <- read.csv("results/step06-all-pairwise-interactions.csv")
gozh_g    <- read.csv("results/step07-gozh-global.csv")
gozh_l    <- read.csv("results/step07-local-gozh.csv")

# --- Global q per variable -----------------------------------------------------
cat("Global q per variable (OPGD-style,", n_strata, "strata):\n")
global_var_df <- do.call(rbind, lapply(all_vars, function(v) {
  res <- run_global_gd(d, resp, v, n_strata = n_strata)
  cat(sprintf("  %-24s q = %.4f  p = %.4g\n", v, res["q"], res["p"]))
  data.frame(Variable = v, Global_q = res["q"], Global_p = res["p"],
             Variable_type = ifelse(grepl("^gc_", v), "GC", "X"),
             stringsAsFactors = FALSE)
}))

# --- Global q for the strongest interaction pairs ------------------------------
top_pairs <- inter_all %>%
  group_by(var1, var2) %>%
  summarise(LPI_mean = mean(qv12, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(LPI_mean)) %>%
  slice_head(n = cfg$params$global$top_n_interactions)

cat("\nGlobal q per interaction pair:\n")
global_inter_df <- do.call(rbind, lapply(seq_len(nrow(top_pairs)), function(i) {
  v1 <- top_pairs$var1[i]; v2 <- top_pairs$var2[i]
  res <- run_global_interaction_q(d, resp, v1, v2, n_strata = n_strata)
  cat(sprintf("  %-40s q = %.4f\n", paste(v1, "x", v2), res["q"]))
  data.frame(Variable = paste(v1, "×", v2),
             Global_q = res["q"], Global_p = res["p"],
             Variable_type = "Interaction", stringsAsFactors = FALSE)
}))

global_gozh_df <- data.frame(
  Variable = "All variables (GOZH)",
  Global_q = gozh_g$q_value[1], Global_p = gozh_g$p_value[1],
  Variable_type = "Total", stringsAsFactors = FALSE
)

global_q_all <- rbind(global_var_df, global_inter_df, global_gozh_df)
write.csv(global_q_all, "results/step08-global-q.csv", row.names = FALSE)

# --- Validation table: local summary + global benchmark ------------------------
q_summary <- do.call(rbind, lapply(all_vars, function(v) {
  q_vals <- local_q[[v]]; sig_vals <- local_sig[[v]]
  data.frame(
    Variable      = v,
    LPI_mean      = round(mean(q_vals, na.rm = TRUE), 4),
    LPI_min       = round(min(q_vals,  na.rm = TRUE), 4),
    LPI_max       = round(max(q_vals,  na.rm = TRUE), 4),
    Pct_sig_p05   = round(mean(sig_vals < 0.05, na.rm = TRUE) * 100, 1),
    Variable_type = ifelse(grepl("^gc_", v), "GC", "X"),
    stringsAsFactors = FALSE
  )
}))

inter_summary <- inter_all %>%
  group_by(var1, var2) %>%
  summarise(
    LPI_mean    = round(mean(qv12, na.rm = TRUE), 4),
    LPI_min     = round(min(qv12,  na.rm = TRUE), 4),
    LPI_max     = round(max(qv12,  na.rm = TRUE), 4),
    Pct_sig_p05 = round(mean(pv12 < 0.05, na.rm = TRUE) * 100, 1),
    .groups     = "drop"
  ) %>%
  arrange(desc(LPI_mean)) %>%
  slice_head(n = cfg$params$global$top_n_interactions) %>%
  mutate(Variable = paste(var1, "×", var2),
         Variable_type = "Interaction") %>%
  select(Variable, LPI_mean, LPI_min, LPI_max, Pct_sig_p05, Variable_type)

gozh_row <- data.frame(
  Variable      = "All variables (GOZH)",
  LPI_mean      = round(mean(gozh_l$local_gozh, na.rm = TRUE), 4),
  LPI_min       = round(min(gozh_l$local_gozh,  na.rm = TRUE), 4),
  LPI_max       = round(max(gozh_l$local_gozh,  na.rm = TRUE), 4),
  Pct_sig_p05   = round(mean(gozh_l$local_gozh_p < 0.05, na.rm = TRUE) * 100, 1),
  Variable_type = "Total",
  stringsAsFactors = FALSE
)

validation <- rbind(q_summary, as.data.frame(inter_summary), gozh_row) %>%
  left_join(global_q_all[, c("Variable", "Global_q", "Global_p")],
            by = "Variable")

write.csv(validation, "results/step08-validation-table.csv", row.names = FALSE)
cat("\nSaved: results/step08-global-q.csv, results/step08-validation-table.csv\n")
print(head(validation %>% arrange(desc(LPI_mean)), 15))
