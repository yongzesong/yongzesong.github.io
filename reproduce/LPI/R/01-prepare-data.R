# =============================================================================
# 01-prepare-data.R — build the analysis table
#
# Output: data/processed/analysis-table.csv with EXACTLY this schema:
#   <id_col>   unique unit id                     (config: data$id_col)
#   <x_col>    longitude or easting               (config: data$x_col)
#   <y_col>    latitude or northing               (config: data$y_col)
#   y          the observed response variable     (config: response$direct_col)
#   x1..xp     the explanatory variables listed in predictors$x_vars
#
# Two modes (config: data$use_example):
#   true  — simulate a synthetic demo dataset so the whole pipeline runs
#           end to end with no external data (recommended first run)
#   false — adapt the marked section below to your own raw data
# =============================================================================

source("R/functions/fn-config.R")
cfg <- load_config()
ensure_dir("data/processed")

if (isTRUE(cfg$data$use_example)) {

  # ---------------------------------------------------------------------------
  # EXAMPLE MODE — synthetic spatial dataset (n = 600 point units)
  # Design: two latent regimes (west/east) with different dominant drivers,
  # so local q values genuinely vary across space; x5 x x6 carry a joint
  # (interaction) signal; y is the directly observed response.
  # ---------------------------------------------------------------------------
  set.seed(42)
  n <- 600

  lon <- runif(n, 116.0, 117.0)   # ~100 km east-west extent
  lat <- runif(n, 36.0, 36.9)

  east <- (lon - min(lon)) / diff(range(lon))   # 0 west -> 1 east
  north <- (lat - min(lat)) / diff(range(lat))

  smooth_field <- function(fx, fy, phase = 0, noise = 0.15) {
    field <- sin(2 * pi * fx * east + phase) * cos(2 * pi * fy * north) +
      0.5 * north
    as.numeric(scale(field + rnorm(n, 0, noise)))
  }

  x1 <- smooth_field(1.0, 0.5)
  x2 <- smooth_field(0.5, 1.0, phase = 1)
  x3 <- smooth_field(1.5, 0.7, phase = 2)
  x4 <- as.numeric(scale(east + rnorm(n, 0, 0.3)))
  x5 <- smooth_field(0.8, 1.2, phase = 3)
  x6 <- smooth_field(1.2, 0.4, phase = 4)

  # Observed response: driver x1 dominates in the west, x2 in the east,
  # x5*x6 adds a nonlinear-enhance interaction, x3 a weak global effect.
  latent <- (1 - east) * 1.2 * x1 + east * 1.2 * x2 +
    0.8 * x5 * x6 + 0.3 * x3 + rnorm(n, 0, 0.4)
  y_obs <- (latent - min(latent)) / diff(range(latent))

  d <- data.frame(
    uid = seq_len(n),
    lon = lon,
    lat = lat,
    y   = y_obs,
    x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5, x6 = x6
  )

  write.csv(d, cfg$data$analysis_table, row.names = FALSE)
  cat(sprintf("Example analysis table written: %s (%d rows)\n",
              cfg$data$analysis_table, nrow(d)))
  cat("Set data$use_example to false in config/project-config.yaml\n")
  cat("and adapt this script once your own data are ready.\n")

} else {

  # ---------------------------------------------------------------------------
  # REAL-DATA MODE — adapt this section to your domain.
  # Typical steps:
  #   1. read raw files from data/raw/
  #   2. aggregate to your spatial unit (grid cell, county, segment, station)
  #   3. compute or join coordinates for each unit
  #   4. rename columns to match config (id, coords, response, x_vars)
  #   5. drop units with missing coordinates; document any imputation
  # ---------------------------------------------------------------------------
  raw_path <- "data/raw/raw-data.csv"   # EDIT: your raw source file
  if (!file.exists(raw_path)) {
    stop("Real-data mode: expected raw data at '", raw_path,
         "'. Edit R/01-prepare-data.R for your source format.")
  }
  raw <- read.csv(raw_path)

  # EDIT from here: aggregation and renaming for your domain. The example
  # below simply passes columns through and keeps complete rows.
  needed <- c(cfg$data$id_col, cfg$data$x_col, cfg$data$y_col,
              cfg$response$direct_col, cfg$predictors$x_vars)
  missing_cols <- setdiff(needed, names(raw))
  if (length(missing_cols) > 0) {
    stop("Raw data lacks required columns: ",
         paste(missing_cols, collapse = ", "),
         ". Adapt R/01-prepare-data.R or config/project-config.yaml.")
  }

  d <- raw[complete.cases(raw[, needed]), needed]
  write.csv(d, cfg$data$analysis_table, row.names = FALSE)
  cat(sprintf("Analysis table written: %s (%d rows, %d dropped as incomplete)\n",
              cfg$data$analysis_table, nrow(d), nrow(raw) - nrow(d)))
}
