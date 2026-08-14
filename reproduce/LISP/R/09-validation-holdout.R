# =============================================================================
# 09-validation-holdout.R — subsample stability of local and global q
#
# Robustness protocol from the cc005 R1 revision: randomly drop a share of
# units (default 5%), recompute local q (LISP) and global q for the top-N
# variables, repeat n_repeats times. Reports per-variable mean local q with
# 95% CI, the median change rate against the full-data run, and Spearman
# rank preservation of the variable-importance ordering.
#
# Outputs: results/step09-holdout-local-q.csv
#          results/step09-holdout-global-q.csv
#          results/step09-holdout-summary.csv
# =============================================================================

source("R/functions/fn-config.R")
source("R/functions/fn-lpi-core.R")

library(sf)
library(localsp)
library(gdverse)
library(sdsfun)
library(dplyr)

cfg <- load_config()
ensure_dir("results")

hold      <- cfg$validation$holdout
set.seed(hold$seed)

d        <- build_model_frame(cfg)
all_vars <- all_vars_of(cfg)
resp     <- cfg$response$response_name
n_cores  <- get_cores(cfg)

# Top-N variables by full-data mean local q.
local_q_full <- read.csv("results/step05-local-q-values.csv")
mean_q_full  <- sort(colMeans(local_q_full[, all_vars], na.rm = TRUE),
                     decreasing = TRUE)
top_vars <- names(mean_q_full)[1:hold$top_n_vars]

cat(sprintf("Holdout validation: %d repeats, drop %.0f%%, top %d variables\n",
            hold$n_repeats, hold$drop_share * 100, hold$top_n_vars))

rep_local  <- list()
rep_global <- list()

for (r in seq_len(hold$n_repeats)) {
  cat(sprintf("Repeat %d / %d\n", r, hold$n_repeats))
  keep_idx <- sort(sample(seq_len(nrow(d)),
                          size = round(nrow(d) * (1 - hold$drop_share))))
  d_sub <- d[keep_idx, ]

  # Rebuild spatial scaffolding and local extent on the subsample.
  spatial_sub <- build_spatial(d_sub, cfg)
  lr_sub      <- estimate_local_range(d_sub, cfg, spatial_sub)

  lisp_sub <- run_lisp_factor(
    d_sub, cfg, vars = top_vars,
    threshold = lr_sub$threshold,
    distmat   = spatial_sub$distmat,
    cores     = n_cores
  )

  rep_local[[r]] <- data.frame(
    repeat_id = r,
    Variable  = top_vars,
    mean_local_q = sapply(top_vars, function(v) {
      mean(lisp_sub$q[[v]], na.rm = TRUE)
    })
  )

  rep_global[[r]] <- data.frame(
    repeat_id = r,
    Variable  = top_vars,
    global_q  = sapply(top_vars, function(v) {
      run_global_gd(d_sub, resp, v,
                    n_strata = cfg$params$global$n_strata)["q"]
    })
  )
}

local_df  <- do.call(rbind, rep_local)
global_df <- do.call(rbind, rep_global)
write.csv(local_df,  "results/step09-holdout-local-q.csv",  row.names = FALSE)
write.csv(global_df, "results/step09-holdout-global-q.csv", row.names = FALSE)

# --- Summary: CI, change rate, rank preservation -------------------------------
full_ref <- data.frame(Variable = top_vars,
                       full_mean_q = as.numeric(mean_q_full[top_vars]))

summary_df <- local_df %>%
  group_by(Variable) %>%
  summarise(
    sub_mean_q = mean(mean_local_q),
    ci_lower   = quantile(mean_local_q, 0.025),
    ci_upper   = quantile(mean_local_q, 0.975),
    .groups    = "drop"
  ) %>%
  left_join(full_ref, by = "Variable") %>%
  mutate(change_pct = round((sub_mean_q - full_mean_q) / full_mean_q * 100, 2))

# Spearman rank correlation of variable importance, per repeat.
rank_cor <- local_df %>%
  left_join(full_ref, by = "Variable") %>%
  group_by(repeat_id) %>%
  summarise(spearman = cor(mean_local_q, full_mean_q, method = "spearman"),
            .groups = "drop")

summary_df$spearman_median <- median(rank_cor$spearman)
summary_df$spearman_min    <- min(rank_cor$spearman)

write.csv(summary_df, "results/step09-holdout-summary.csv", row.names = FALSE)
cat("Saved: step09-holdout-local-q / step09-holdout-global-q / step09-holdout-summary\n")
print(as.data.frame(summary_df))
cat(sprintf("\nSpearman rank preservation: median = %.3f, min = %.3f\n",
            median(rank_cor$spearman), min(rank_cor$spearman)))
