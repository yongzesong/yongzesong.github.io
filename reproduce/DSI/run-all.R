# =============================================================================
# run-all.R — reproduce every result and figure in one command
# =============================================================================
#   Rscript run-all.R              full pipeline
#   Rscript run-all.R 30 40        only the named steps
#   Rscript run-all.R figures      only the figure scripts
#   Rscript run-all.R test         only the verification tests
#
# Steps write to data/derived/, results/, tables/ and figs/. Each step reads
# what the previous one wrote, so a single step can be re-run after a config
# change without repeating the whole pipeline.
# =============================================================================

t_start <- Sys.time()

# Run from the project root regardless of where Rscript was invoked.
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
if (length(.file)) setwd(dirname(normalizePath(.file)))

source("R/00-config.R")

STEPS <- c(
  "10" = "R/10-prepare-data.R",
  "20" = "R/20-fit-models.R",
  "30" = "R/30-compute-dsi.R",
  "40" = "R/40-compute-dg.R",
  "50" = "R/50-sensitivity.R",
  "60" = "R/60-tables.R"
)

FIGURES <- c(
  "R/p01-study-area.R",
  "R/p02-accuracy.R",
  "R/p03-dsi-theta.R",
  "R/p04-dsi-vs-accuracy.R",
  "R/p05-dg-maps.R",
  "R/p06-sensitivity.R"
)

TESTS <- c(
  "tests/test-01-reproduce-dsi-paper.R",
  "tests/test-02-metric-properties.R"
)

args <- commandArgs(trailingOnly = TRUE)

run_scripts <- function(paths) {
  for (p in paths) {
    if (!file.exists(p)) { log_warn("missing script: %s", p); next }
    ok <- tryCatch({ source(p, local = new.env()); TRUE },
                   error = function(e) {
                     log_warn("%s failed: %s", basename(p), conditionMessage(e))
                     FALSE
                   })
    if (!ok) FAILED <<- c(FAILED, basename(p))
  }
}

FAILED <- character(0)

cat(sprintf("\n%s\nProject : %s\nDomain  : %s\nStarted : %s\n%s\n",
            strrep("=", 66), PROJECT_ID, DOMAIN,
            format(t_start, "%Y-%m-%d %H:%M:%S"), strrep("=", 66)))

if (!length(args)) {
  run_scripts(STEPS)
  run_scripts(FIGURES)
} else if (identical(args[1], "figures")) {
  run_scripts(FIGURES)
} else if (identical(args[1], "test")) {
  run_scripts(TESTS)
} else {
  sel <- STEPS[args[args %in% names(STEPS)]]
  if (!length(sel)) stop("Unknown step(s): ", paste(args, collapse = ", "),
                         "\nValid steps: ", paste(names(STEPS), collapse = ", "),
                         ", figures, test")
  run_scripts(sel)
}

elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
cat(sprintf("\n%s\nFinished in %.1f s\n", strrep("=", 66), elapsed))

if (length(FAILED)) {
  cat("Failed: ", paste(FAILED, collapse = ", "), "\n")
} else {
  cat("Outputs: results/  tables/  figs/  data/derived/\n")
  cat("Next   : read results/dsi-diagnostics.txt, then draft the Results\n")
  cat("         section against manuscript/writing-guide.md\n")
}
