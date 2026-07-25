# =============================================================================
# 20-srii.R — entropy-weighted quantity and quality indices (Eq. 3-6)
# =============================================================================
# Standardizes every indicator with its sign, computes entropy weights within
# each category, then builds the category indices, Gamma_A, Gamma_B and the
# composite SRII per block. The paper's Table 2 and Fig. 4 rest on this step.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("20  SRII: entropy-weighted indices")

blocks <- read_result(F_BLOCKS)
srii <- stor_srii(blocks, INDICATORS)

write_result(srii$weights, F_WEIGHTS)

out <- data.frame(blocks[, c("block_id", "col", "row", "x_km", "y_km")],
                  srii$categories,
                  gamma_a = srii$gamma_a, gamma_b = srii$gamma_b,
                  gamma = srii$gamma)
write_result(out, F_SRII)

log_info("Gamma_A mean %.3f (sd %.3f), Gamma_B mean %.3f (sd %.3f)",
         mean(out$gamma_a), sd(out$gamma_a),
         mean(out$gamma_b), sd(out$gamma_b))
for (cn in unique(srii$weights$category))
  log_info("category %-16s weights: %s", cn,
           paste(sprintf("%.2f", srii$weights$weight[srii$weights$category == cn]),
                 collapse = " "))
