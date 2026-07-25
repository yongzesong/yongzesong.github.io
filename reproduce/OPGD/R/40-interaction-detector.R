# =============================================================================
# 40-interaction-detector.R — interactive effect of every pair of variables
#
# For each pair the interaction detector compares the joint Q(x1 n x2) against
# the two single Q values and classifies the effect into one of five types
# (nonlinear-weaken, uni-weaken, bi-enhance, independent, nonlinear-enhance).
#
# Output: results/interaction-detector.csv
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_GD()
if (!isTRUE(RUN_INTERACTION)) { log_info("RUN_INTERACTION is FALSE; skipping"); } else {

log_head("Step 4/7  Interaction detector")
dcz <- read_result(file.path(DERIVED, "discretized-data.csv"))

gi <- gdinteract(stats::as.formula(paste(RESPONSE, "~ .")), data = dcz)
it <- gi$Interaction
it$qv1  <- round(it$qv1, 4); it$qv2 <- round(it$qv2, 4); it$qv12 <- round(it$qv12, 4)
it <- it[order(-it$qv12), ]
write_result(it, F_INTERACT)
top <- it[1, ]
log_info("strongest interaction: %s n %s -> Q = %.3f (%s)",
         top$var1, top$var2, top$qv12, top$interaction)
log_info("all %d pairs are enhancing (Q of pair exceeds both singles): %s",
         nrow(it), all(grepl("Enhance", it$interaction)))
}
