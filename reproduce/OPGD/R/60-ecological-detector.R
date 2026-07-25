# =============================================================================
# 60-ecological-detector.R — is one variable a significantly stronger driver
# than another?
#
# The ecological detector runs an F-test between every pair of variables to ask
# whether their contributions to spatial stratified heterogeneity differ
# significantly ("Y") or not ("N").
#
# Output: results/ecological-detector.csv
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_GD()
if (!isTRUE(RUN_ECOLOGICAL)) { log_info("RUN_ECOLOGICAL is FALSE; skipping"); } else {

log_head("Step 6/7  Ecological detector")
dcz <- read_result(file.path(DERIVED, "discretized-data.csv"))

ge <- gdeco(stats::as.formula(paste(RESPONSE, "~ .")), data = dcz)
eco <- ge$Ecological
names(eco) <- c("variable1", "variable2", "significant_difference")
write_result(eco, F_ECO)
log_info("%d of %d variable pairs differ significantly in their impact",
         sum(eco$significant_difference == "Y"), nrow(eco))
}
