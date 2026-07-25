# =============================================================================
# 50-risk-detector.R — where the response is high vs low, and whether the
# difference between strata is significant
#
# riskmean() gives the mean response within each stratum of every variable;
# gdrisk() runs a t-test between every pair of strata and flags significant
# differences. Together they turn a Q value into an actionable "which zone".
#
# Outputs: results/risk-means.csv      (stratum means, long)
#          results/risk-detector.csv    (pairwise t-tests, long)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_GD()
if (!isTRUE(RUN_RISK)) { log_info("RUN_RISK is FALSE; skipping"); } else {

log_head("Step 5/7  Risk detector")
dcz <- read_result(file.path(DERIVED, "discretized-data.csv"))

rm_obj <- riskmean(stats::as.formula(paste(RESPONSE, "~ .")), data = dcz)
means <- do.call(rbind, lapply(names(rm_obj), function(v) {
  x <- rm_obj[[v]]; if (is.null(x)) return(NULL)
  data.frame(variable = v, stratum = as.character(x$itv),
             mean_response = round(x$meanrisk, 4), row.names = NULL)
}))
write_result(means, F_RISKMEAN)

gr <- gdrisk(stats::as.formula(paste(RESPONSE, "~ .")), data = dcz)
risk <- do.call(rbind, lapply(names(gr), function(v) {
  x <- gr[[v]]; if (is.null(x)) return(NULL)
  data.frame(variable = v, stratum1 = as.character(x$itv1),
             stratum2 = as.character(x$itv2),
             t = round(x$t, 3), sig = signif(x$sig, 3),
             risk = as.character(x$risk), row.names = NULL)
}))
write_result(risk, F_RISK)
log_info("%d stratum pairs tested; %d significantly different (t-test)",
         nrow(risk), sum(risk$risk == "Y"))
}
