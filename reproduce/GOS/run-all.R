# =============================================================================
# run-all.R — reproduce every GOS result, figure and table in one command
# =============================================================================
#   Rscript run-all.R              full pipeline
#   Rscript run-all.R 30 40        only the named steps
#   Rscript run-all.R test         only the verification tests
#
# Every step writes to data/derived/, results/, tables/ and figs/, and reads
# what the previous one wrote, so a single step can be re-run after a change in
# config/project-config.R without repeating the whole pipeline. Each numbered
# step also draws its own figures.
#
# Per-step wall-clock times are appended to env/runtimes.csv.
# =============================================================================

t_start <- Sys.time()

.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
if (length(.file)) setwd(dirname(normalizePath(.file)))

source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

STEPS <- c(
  "10" = "R/10-prepare-data.R",
  "20" = "R/20-select-variables.R",
  "30" = "R/30-best-kappa.R",
  "40" = "R/40-gos-prediction.R",
  "50" = "R/50-model-comparison.R",
  "90" = "R/90-tables.R"
)
TESTS <- c("tests/test-01-method-properties.R",
           "tests/test-02-reproduce-pipeline.R")

args   <- commandArgs(trailingOnly = TRUE)
FAILED <- character(0)
TIMES  <- data.frame(script = character(0), seconds = numeric(0),
                     stringsAsFactors = FALSE)

run_scripts <- function(paths, record = TRUE) {
  for (p in paths) {
    if (!file.exists(p)) { log_warn("missing script: %s", p); next }
    t0 <- Sys.time()
    ok <- tryCatch({ source(p, local = new.env()); TRUE },
                   error = function(e) {
                     log_warn("%s failed: %s", basename(p), conditionMessage(e)); FALSE })
    secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (record)
      TIMES <<- rbind(TIMES, data.frame(script = basename(p), seconds = round(secs, 1),
                                        stringsAsFactors = FALSE))
    if (!ok) FAILED <<- c(FAILED, basename(p))
  }
}

cat(sprintf("\n%s\nProject : %s\nDomain  : %s\nMethod  : %s\n%s\n",
            strrep("=", 74), PROJECT_ID, DOMAIN, METHOD_PAPER, strrep("=", 74)))

if (!length(args)) {
  run_scripts(STEPS)
} else if (identical(args[1], "test")) {
  run_scripts(TESTS, record = FALSE)
} else {
  sel <- STEPS[args[args %in% names(STEPS)]]
  if (!length(sel)) stop("Unknown step(s): ", paste(args, collapse = ", "),
                         "\nValid: ", paste(names(STEPS), collapse = ", "), ", test")
  run_scripts(sel)
}

elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
if (nrow(TIMES)) {
  # A partial run must not wipe the timings of the steps it did not touch:
  # its rows are merged into whatever env/runtimes.csv already holds, and
  # TOTAL is recomputed from the merged per-step seconds.
  RT_FILE <- file.path(ENV_DIR, "runtimes.csv")
  prev <- if (file.exists(RT_FILE)) utils::read.csv(RT_FILE, stringsAsFactors = FALSE)
          else TIMES[0, ]
  prev <- prev[prev$script != "TOTAL" & !(prev$script %in% TIMES$script), , drop = FALSE]
  TIMES <- rbind(prev, TIMES)
  TIMES <- TIMES[order(match(TIMES$script, basename(STEPS))), , drop = FALSE]
  TIMES <- rbind(TIMES, data.frame(script = "TOTAL",
                                   seconds = round(sum(TIMES$seconds), 1)))
  utils::write.csv(TIMES, RT_FILE, row.names = FALSE)
  cat("\n"); print(TIMES, row.names = FALSE)
  cat(sprintf("(this invocation: %.1f s wall clock)\n", elapsed))
}
cat(sprintf("\n%s\nFinished in %.1f s\n", strrep("=", 74), elapsed))
if (length(FAILED)) cat("Failed: ", paste(FAILED, collapse = ", "), "\n") else
  cat("Outputs: results/  tables/  figs/  data/  env/\n")
