# =============================================================================
# 09-make-synthetic-data.R — a spatially structured dataset with known truth
# =============================================================================
# Runs when INPUT_FILE is NULL, so the pipeline is executable before any real
# data exists. It also serves the role of the simulation experiment in Section
# 5.1 of the DSI paper: because the data-generating process is known, a model
# that recovers it should score a high DSI and a model that ignores space
# should not.
#
# The generated field carries both spatial characteristics DSI measures:
#   - autocorrelation, from a smooth distance-decayed random field
#   - heterogeneity, from a regime term that shifts the response by region
# plus a spatially correlated error term, so residual structure is present by
# construction rather than by accident.
# =============================================================================

make_synthetic_data <- function(n_side = 32, seed = SEED,
                                lon_range = c(114, 154), lat_range = c(-39, -11),
                                noise_sd = 1.0, range_km = 250) {
  set.seed(seed)

  grid <- expand.grid(
    lon = seq(lon_range[1], lon_range[2], length.out = n_side),
    lat = seq(lat_range[1], lat_range[2], length.out = n_side)
  )
  n <- nrow(grid)

  # Smooth spatial fields from an exponential covariance, giving predictors
  # that are themselves autocorrelated the way environmental covariates are.
  d_km <- as.matrix(stats::dist(cbind(grid$lon * 95, grid$lat * 111)))
  smooth_field <- function(range_km, sd = 1) {
    covm <- sd^2 * exp(-d_km / range_km)
    L <- chol(covm + diag(1e-6, n))
    as.numeric(t(L) %*% stats::rnorm(n))
  }

  temperature   <- 18 - 0.35 * (grid$lat + 25) + smooth_field(range_km, 1.5)
  precipitation <- 600 + 90 * smooth_field(range_km * 1.4, 1)
  elevation     <- 300 + 120 * smooth_field(range_km * 0.6, 1)
  soil_ph       <- 6.5 + 0.4 * smooth_field(range_km * 0.8, 1)
  distance_km   <- as.numeric(scale(smooth_field(range_km * 1.1, 1))) * 50 + 150
  noise_var     <- stats::rnorm(n)                       # a predictor with no signal

  # Regime term: three east-west bands with different intercepts. This is the
  # heterogeneity component, and it is what the Q value picks up.
  regime <- cut(grid$lon, breaks = 3, labels = FALSE)
  regime_effect <- c(-4, 0, 5)[regime]

  # Spatially correlated error, so a well-fitting aspatial model still leaves
  # structure in its residuals.
  spatial_error <- 2.2 * smooth_field(range_km * 0.5, 1)

  y <- 12 +
    0.9 * (temperature - mean(temperature)) +
    0.02 * (precipitation - mean(precipitation)) +
    0.006 * (elevation - mean(elevation)) +
    1.5 * (soil_ph - mean(soil_ph)) +
    -0.015 * (distance_km - mean(distance_km)) +
    regime_effect +
    spatial_error +
    stats::rnorm(n, sd = noise_sd)

  out <- data.frame(
    lon = grid$lon, lat = grid$lat, y = y,
    temperature = temperature, precipitation = precipitation,
    elevation = elevation, soil_ph = soil_ph,
    distance_km = distance_km, noise_var = noise_var
  )
  attr(out, "truth") <- list(
    regime = regime, spatial_error = spatial_error,
    informative = c("temperature", "precipitation", "elevation", "soil_ph", "distance_km"),
    uninformative = "noise_var"
  )
  out
}
