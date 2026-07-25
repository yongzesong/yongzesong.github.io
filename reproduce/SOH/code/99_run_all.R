# =============================================================================
# 99_run_all.R — run the whole pipeline end to end
# =============================================================================
#   Rscript code/99_run_all.R
# or, from inside R with the working directory anywhere in the project,
#   source("code/99_run_all.R")
#
# Every step reads its input from data/interim/ and writes its output there,
# so any single step can be re-run on its own after a configuration change.
# =============================================================================

.a <- commandArgs(trailingOnly = FALSE)
.p <- sub("^--file=", "", .a[grep("^--file=", .a)])
CODE <- if (length(.p)) dirname(normalizePath(.p)) else {
  c("code", ".", "../code")[file.exists(file.path(
    c("code", ".", "../code"), "00_setup.R"))][1]
}

source(file.path(CODE, "00_setup.R"))

soh_log(strrep("=", 62))
soh_log("SOH pipeline: ", cfg$study$title)
soh_log("study id: ", cfg$study$id, "   data mode: ", cfg$data$mode)
soh_log(strrep("=", 62))

t0 <- Sys.time()
steps <- c("01_prepare_data.R", "02_generate_sops.R", "03_pd_individual.R",
           "04_pd_interactions.R", "05_validation.R", "06_sensitivity.R",
           "07_figures.R")

for (s in steps) {
  ok <- tryCatch({ source(file.path(CODE, s)); TRUE },
                 error = function(e) { soh_log(s, " FAILED: ",
                                               conditionMessage(e), level = "ERROR"); FALSE })
  if (!ok) stop("Pipeline halted at ", s)
}

soh_record_session()
soh_log(strrep("=", 62))
soh_log("pipeline complete in ",
        round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2), " minutes")
soh_log("tables:  03_results/tables/")
soh_log("figures: 03_results/figures/")
soh_log("Next: fill 04_manuscript/01-abstract-worksheet.md with the numbers ",
        "in T04, T09, T10, and T12.")
