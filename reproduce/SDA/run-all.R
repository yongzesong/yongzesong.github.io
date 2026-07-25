# =============================================================================
# run-all.R — reproduce every SDA result and figure in one command
# =============================================================================
#   Rscript run-all.R              full pipeline
#   Rscript run-all.R 30 40        only the named steps
#   Rscript run-all.R figures      only the figure scripts
#   Rscript run-all.R test         only the verification tests
#
# Steps write to data/derived/, results/, tables/ and figs/. Each step reads
# what the previous one wrote, so a single step can be re-run after a config
# change without repeating the whole pipeline.
#
# The method itself lives in R/02-sda-core.R and depends on no package.
# =============================================================================

t_start <- Sys.time()

.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
if (length(.file)) setwd(dirname(normalizePath(.file)))

source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

STEPS <- c(
  "10" = "R/10-prepare-data.R",
  "20" = "R/20-generate-sdvars.R",
  "30" = "R/30-select-variables.R",
  "40" = "R/40-cross-validation.R",
  "50" = "R/50-buffer-sensitivity.R",
  "90" = "R/90-tables.R"
)
FIGURES <- c("R/p01-selected-vars.R", "R/p02-cross-validation.R",
             "R/p03-buffer-study.R")
TESTS   <- c("tests/test-01-reproduce-published.R",
             "tests/test-02-method-properties.R")

args <- commandArgs(trailingOnly = TRUE)
FAILED <- character(0)

run_scripts <- function(paths) {
  for (p in paths) {
    if (!file.exists(p)) { log_warn("missing script: %s", p); next }
    ok <- tryCatch({ source(p, local = new.env()); TRUE },
                   error = function(e) {
                     log_warn("%s failed: %s", basename(p), conditionMessage(e)); FALSE })
    if (!ok) FAILED <<- c(FAILED, basename(p))
  }
}

cat(sprintf("\n%s\nProject : %s\nDomain  : %s\n%s\n",
            strrep("=", 66), PROJECT_ID, DOMAIN, strrep("=", 66)))

if (!length(args)) {
  run_scripts(STEPS); run_scripts(FIGURES)
} else if (identical(args[1], "figures")) {
  run_scripts(FIGURES)
} else if (identical(args[1], "test")) {
  run_scripts(TESTS)
} else {
  sel <- STEPS[args[args %in% names(STEPS)]]
  if (!length(sel)) stop("Unknown step(s): ", paste(args, collapse = ", "),
                         "\nValid: ", paste(names(STEPS), collapse = ", "),
                         ", figures, test")
  run_scripts(sel)
}

elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
cat(sprintf("\n%s\nFinished in %.1f s\n", strrep("=", 66), elapsed))
if (length(FAILED)) cat("Failed: ", paste(FAILED, collapse = ", "), "\n") else
  cat("Outputs: results/  tables/  figs/  data/derived/\n")
