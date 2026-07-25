# =============================================================================
# 40-lisa.R — hotspots, cold spots and trade-off groups (Section 3.3)
# =============================================================================
# Local Moran's I on Gamma_A and Gamma_B with queen contiguity, significant
# HH / LL clusters at LISA_ALPHA, then the overlay: joint hotspots, joint cold
# spots, and a development strategy for every cold-side block (Fig. 8).
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("40  LISA: clusters and trade-off groups")

srii <- read_result(F_SRII)
nb <- stor_grid_neighbors(srii$col, srii$row)

lm_a <- stor_local_moran(srii$gamma_a, nb, nsim = LISA_NSIM, seed = SEED)
lm_b <- stor_local_moran(srii$gamma_b, nb, nsim = LISA_NSIM, seed = SEED + 1)
class_a <- stor_lisa_class(lm_a, LISA_ALPHA)
class_b <- stor_lisa_class(lm_b, LISA_ALPHA)

groups <- stor_tradeoff_groups(class_a, class_b)

lisa <- data.frame(srii[, c("block_id", "col", "row", "x_km", "y_km")],
                   gamma_a = srii$gamma_a, gamma_b = srii$gamma_b,
                   Ii_a = lm_a$Ii, p_a = lm_a$p, class_a = class_a,
                   Ii_b = lm_b$Ii, p_b = lm_b$p, class_b = class_b,
                   cluster = groups$cluster, strategy = groups$strategy)
write_result(lisa, F_LISA)

tab <- as.data.frame(table(cluster = groups$cluster, strategy = groups$strategy))
tab <- tab[tab$Freq > 0, ]
write_result(tab, F_TRADEOFF)

log_info("clusters: %s",
         paste(sprintf("%s %d", names(table(groups$cluster)),
                       table(groups$cluster)), collapse = ", "))
log_info("strategies (cold-side blocks): %s",
         paste(sprintf("%s %d", names(table(groups$strategy)),
                       table(groups$strategy)), collapse = ", "))
