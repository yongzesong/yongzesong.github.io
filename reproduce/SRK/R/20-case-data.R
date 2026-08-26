# =============================================================================
# 20-case-data.R — fetch and screen the Western Australian case-study samples
#
# The GSWA geochemistry samples and their lithological/terrain covariates are
# not redistributed with this tutorial; this step downloads the sample table
# the authors released for each element in ELEMENTS, checks its shape, and
# re-runs the Spearman screen of Sec. 4.2 that decides which covariates it keeps.
#
# Set DATA_SOURCE <- "local" in config/project-config.R and put your own CSVs
# in data/ to run everything below on your own study area.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("20  Case-study data and covariate screening")
t0 <- Sys.time()

fetch <- function(fname) {
  dest <- file.path(DATA_DIR, fname)
  if (file.exists(dest)) { log_info("have %s", fname); return(dest) }
  if (!identical(DATA_SOURCE, "download"))
    stop("DATA_SOURCE is 'local' but data/", fname, " is missing")
  url <- paste0(DATA_BASE_URL, "/", fname)
  log_info("downloading %s", fname)
  ok <- tryCatch({ utils::download.file(url, dest, quiet = TRUE, mode = "wb")
                   TRUE },
                 error = function(e) { log_warn("%s", conditionMessage(e)); FALSE })
  if (!ok || !file.exists(dest))
    stop("could not download ", url, "\nFetch it by hand into data/ and re-run.")
  dest
}

# -- One tidy table per element ----------------------------------------------
summaries <- list(); screens <- list()

for (elem in names(ELEMENTS)) {
  cfg  <- ELEMENTS[[elem]]
  raw  <- utils::read.csv(fetch(cfg$file), check.names = FALSE)
  yvar <- cfg$yvar
  need <- c(XCOL, YCOL, yvar, CANDIDATE_XVARS)
  miss <- setdiff(need, names(raw))
  if (length(miss)) stop(elem, ": missing column(s) ", paste(miss, collapse = ", "))

  d <- raw[stats::complete.cases(raw[, need]), need]
  names(d)[1:2] <- c("x", "y")

  # Sec. 4.1: repeat measurements at one location are averaged to a single
  # representative sample. The released tables are already averaged, so this
  # normally changes nothing — it is here so the step is safe on raw exports.
  key <- paste(d$x, d$y, sep = "_")
  n_before <- nrow(d)
  if (anyDuplicated(key)) {
    d <- do.call(rbind, lapply(split(d, key), function(g) {
      g[1, ] -> out; out[] <- lapply(names(g), function(cc) mean(g[[cc]])); out
    }))
    log_warn("%s: averaged %d duplicate locations", elem, n_before - nrow(d))
  }
  rownames(d) <- NULL
  write_result(d, F_SAMPLES(elem))
  log_info("%s: %d samples, %.1f-%.1f %s", elem, nrow(d),
           min(d[[yvar]]), max(d[[yvar]]), cfg$unit)

  summaries[[elem]] <- data.frame(
    element = elem, n = nrow(d), unit = cfg$unit,
    mean = mean(d[[yvar]]), sd = stats::sd(d[[yvar]]),
    min = min(d[[yvar]]), median = stats::median(d[[yvar]]),
    max = max(d[[yvar]]),
    cv = stats::sd(d[[yvar]]) / mean(d[[yvar]]),
    skewness = mean((d[[yvar]] - mean(d[[yvar]]))^3) / stats::sd(d[[yvar]])^3,
    extent_x_km = diff(range(d$x)) / 1000, extent_y_km = diff(range(d$y)) / 1000)

  # -- Spearman screen, Sec. 3.2.1 -------------------------------------------
  sc <- do.call(rbind, lapply(CANDIDATE_XVARS, function(xv) {
    ct <- suppressWarnings(stats::cor.test(d[[yvar]], d[[xv]], method = "spearman"))
    data.frame(element = elem, covariate = xv,
               rho = unname(ct$estimate), p = ct$p.value,
               significant = ct$p.value < 0.05,
               used_by_paper = xv %in% cfg$xvars)
  }))
  screens[[elem]] <- sc[order(-abs(sc$rho)), ]
  agree <- identical(sort(sc$covariate[sc$significant]), sort(cfg$xvars))
  log_info("%s: screen keeps %s%s", elem,
           paste(sc$covariate[sc$significant], collapse = ", "),
           if (agree) "  (= the published set)" else
             sprintf("  (published set: %s)", paste(cfg$xvars, collapse = ", ")))
}

write_result(do.call(rbind, summaries), F_RESPONSE)
write_result(do.call(rbind, screens),   F_SCREEN)

# -- Provenance --------------------------------------------------------------
n_files <- length(ELEMENTS)
writeLines(c(
  "# Data provenance",
  "",
  sprintf("The sample %s below %s downloaded by `R/20-case-data.R` from the code",
          if (n_files == 1L) "table" else "tables",
          if (n_files == 1L) "is" else "are"),
  sprintf("repository released with the paper and %s **not** redistributed with this",
          if (n_files == 1L) "is" else "are"),
  "tutorial:",
  "",
  "    https://github.com/renkaigis/Singularity_Regression_Kriging",
  "",
  "| file | rows | what it holds |",
  "|---|---|---|",
  sprintf("| `%s` | %d | %s samples: coordinates, %s concentration, four truncated-linear lithology proximity variables (ss, ifi, imi, hm) and three DEM terrain variables |",
          vapply(ELEMENTS, `[[`, character(1), "file"),
          vapply(names(ELEMENTS), function(e) nrow(read_result(F_SAMPLES(e))), numeric(1)),
          names(ELEMENTS), names(ELEMENTS)),
  "",
  "## Original sources",
  "",
  "* Geochemistry: GSWA Geochemistry database (DMIRS-047), Department of Energy,",
  "  Mines, Industry Regulation and Safety, Western Australia.",
  "* Lithology: Surface Geology of Australia, Geoscience Australia (2012), turned",
  "  into per-unit proximity variables by the truncated linear decay of Eq. 14",
  "  (1 inside the unit, falling to 0 over a 10 km buffer).",
  "* Terrain: elevation, slope and aspect from a Geoscience Australia DEM (2015).",
  "",
  "## Coordinates",
  "",
  "`x` and `y` are Web Mercator metres (EPSG:3857) near 122.5E, 32.5S. At that",
  "latitude Web Mercator inflates distance by 1/cos(32.5 deg) = 1.19, so the",
  "2-20 km singularity scales and the 15 km cross-validation block are about",
  "1.7-16.9 km and 12.6 km on the ground. Every distance in this pipeline is",
  "expressed in the same projected metres as the published analysis.",
  "",
  "## Not downloaded",
  "",
  "`grid_linear.csv` (97,234 cells at 500 m) drives the published prediction",
  "maps. This tutorial stops at cross-validation, so the grid is not needed;",
  "the maps are shown on the page as figures from the article."),
  file.path(DATA_DIR, "provenance.md"))
log_info("wrote data/provenance.md")

record_runtime("20-case-data", as.numeric(difftime(Sys.time(), t0, units = "secs")))
