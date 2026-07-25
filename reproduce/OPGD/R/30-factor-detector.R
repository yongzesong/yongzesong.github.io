# =============================================================================
# 30-factor-detector.R — relative importance of each explanatory variable
#
# The factor detector computes the Q value (q = 1 - SSW/SST) of every variable
# on the optimally discretised table, with a significance test. Higher Q means
# the variable's strata explain more of the spatial stratified heterogeneity.
#
# Output: results/factor-detector.csv  (variable, qv, sig, rank)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_GD()
if (!isTRUE(RUN_FACTOR)) { log_info("RUN_FACTOR is FALSE; skipping"); } else {

log_head("Step 3/7  Factor detector")
dcz <- read_result(file.path(DERIVED, "discretized-data.csv"))

g <- gd(stats::as.formula(paste(RESPONSE, "~ .")), data = dcz)
fac <- g$Factor
fac <- fac[order(-fac$qv), ]
fac$rank <- seq_len(nrow(fac))
names(fac) <- c("variable", "qv", "sig", "rank")
fac$qv  <- round(fac$qv, 4)
write_result(fac, F_FACTOR)
log_info("dominant variable: %s (Q = %.3f); weakest: %s (Q = %.3f)",
         fac$variable[1], fac$qv[1],
         fac$variable[nrow(fac)], fac$qv[nrow(fac)])
}
