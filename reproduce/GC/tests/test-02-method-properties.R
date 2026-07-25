# =============================================================================
# test-02-method-properties.R — properties geocomplexity must have
#
# These are checks on the idea rather than on one dataset: a variable that
# varies smoothly in space is not complex, a variable scattered at random is,
# and the measure must not simply track the variable's own magnitude.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("geocomplexity", "the method"); need_pkg("sf", "geometry")

cat("\n== test-02  Geocomplexity properties ====================================\n")
pass <- 0; total <- 0
check <- function(label, cond) {
  total <<- total + 1; ok <- isTRUE(cond)
  cat(sprintf("   %-58s %s\n", label, if (ok) "pass" else "FAIL")); pass <<- pass + ok
}

## A regular grid of square cells, so neighbourhoods are unambiguous.
set.seed(42)
n <- 12
g <- expand.grid(col = 1:n, row = 1:n)
polys <- lapply(seq_len(nrow(g)), function(i) {
  x0 <- g$col[i]; y0 <- g$row[i]
  sf::st_polygon(list(cbind(c(x0, x0+1, x0+1, x0, x0), c(y0, y0, y0+1, y0+1, y0))))
})
grid <- sf::st_sf(geometry = sf::st_sfc(polys, crs = 4326))
grid$smooth <- g$col                                  # a plane: no local surprise
grid$noise  <- stats::runif(nrow(g))                  # spatially random
wt <- sdsfun::spdep_contiguity_swm(grid, queen = TRUE, style = "B")

gcv <- geocomplexity::geocd_vector(grid[, c("smooth", "noise")], wt = wt,
                                   method = "moran", normalize = TRUE,
                                   returnsf = FALSE)
gcv <- as.data.frame(gcv)
check("a geocomplexity value is returned for every unit and variable",
      nrow(gcv) == nrow(grid) && ncol(gcv) == 2)
check("values are finite", all(is.finite(as.matrix(gcv))))
check("a smooth gradient is less complex than spatial noise",
      mean(gcv$GC_smooth) < mean(gcv$GC_noise))

## Shifting a variable by a constant changes its values but not its arrangement,
## so the complexity of the arrangement must not move.
grid$smooth_shift <- grid$smooth + 1000
g2 <- as.data.frame(geocomplexity::geocd_vector(
        grid[, c("smooth", "smooth_shift")], wt = wt, method = "moran",
        normalize = TRUE, returnsf = FALSE))
check("adding a constant leaves geocomplexity unchanged",
      max(abs(g2$GC_smooth - g2$GC_smooth_shift)) < 1e-8)

## The three measures answer different questions, so they need not agree, but
## each must run and return one value per unit.
for (m in c("moran", "spvar", "shannon")) {
  gm <- try(as.data.frame(geocomplexity::geocd_vector(
              grid[, "noise"], wt = wt, method = m, normalize = TRUE,
              returnsf = FALSE)), silent = TRUE)
  check(sprintf("method '%s' returns one finite value per unit", m),
        !inherits(gm, "try-error") && nrow(gm) == nrow(grid) &&
          all(is.finite(gm[[1]])))
}

## The configuration-similarity form summarises several variables at once.
gs <- try(as.data.frame(geocomplexity::geocs_vector(
            grid[, c("smooth", "noise")], wt = wt, returnsf = FALSE)),
          silent = TRUE)
check("geocs_vector returns a single combined complexity per unit",
      !inherits(gs, "try-error") && nrow(gs) == nrow(grid) && ncol(gs) == 1)

## The weight matrix defines the neighbourhood, so it must change the answer.
wtk <- sdsfun::spdep_contiguity_swm(grid, k = 4, style = "B")
gk <- as.data.frame(geocomplexity::geocd_vector(grid[, "noise"], wt = wtk,
                                                method = "moran",
                                                normalize = TRUE, returnsf = FALSE))
check("a different spatial weight matrix gives a different result",
      max(abs(gk[[1]] - gcv$GC_noise)) > 1e-8)

cat(sprintf("\n   %d passed, %d failed\n", pass, total - pass))
if (pass != total) stop("test-02 had failures")
