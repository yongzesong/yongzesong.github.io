# =========================================================
# DSI core metrics 
# INPUT: CSV with columns: lon, lat, y, residual, + any X columns
# EDIT HERE (only these lines are usually needed):
infile        <- "/Users/oceankevin/Desktop/DSI整理/dsi_input_aus_lm_test.csv"
projected_crs <- 3577   # Australia: 3577; other regions: choose a projected CRS in meters (e.g., UTM)
k_neighbors   <- 8      # Moran's I KNN neighbors (try 6–10 for dense; 10–20 for sparse)
alternative   <- "greater"  # "greater", "less", "two.sided"
# =========================================================

library(sf)
library(spdep)
library(rpart)
library(GD)

# 1) Read data
df <- read.csv(infile, check.names = FALSE, stringsAsFactors = FALSE)
df <- na.omit(df)

# 2) Moran's I (δ0a, δra) using KNN in projected CRS (meters)
pts <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
pts <- st_transform(pts, projected_crs)
xy  <- st_coordinates(pts)

nb <- knn2nb(knearneigh(xy, k = k_neighbors))
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

δ0a <- unname(moran.test(pts$y,        lw, alternative = alternative, zero.policy = TRUE)$estimate[1])
δra <- unname(moran.test(pts$residual, lw, alternative = alternative, zero.policy = TRUE)$estimate[1])

# 3) Q value via GD (δ0h, δrh) using tree-based stratification on X
d <- df
names(d) <- make.names(names(d))  # allow spaces in X names

x_cols <- setdiff(names(d), c("lon", "lat", "y", "residual"))
tree_fit <- rpart(y ~ ., data = d[, c("y", x_cols), drop = FALSE])
d$tree <- as.character(as.numeric(tree_fit$where))

δ0h <- unclass(gd(y ~ tree, d))$Factor$qv
δrh <- unclass(gd(residual ~ tree, d))$Factor$qv

# 4) eta + theta
ηa <- (δ0a - δra) / δ0a
ηh <- (δ0h - δrh) / δ0h

θmin      <- min(ηa, ηh)
θprobable <- max(ηa, ηh)
θmax      <- 1 - (1 - ηa) * (1 - ηh)

# 5) Clean output
out <- data.frame(
  δ0a = δ0a,
  δra = δra,
  δ0h = δ0h,
  δrh = δrh,
  θmin = θmin,
  θprobable = θprobable,
  θmax = θmax,
  check.names = FALSE
)

print(out)
