# =============================================================================
# test-01-reproduce-opgd.R — the code reproduces the published OPGD result on
# the ndvi_40 case of Song et al. (2020). Locks the factor-detector Q values
# (Precipitation and Climate zone dominate NDVI change) within tolerance.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_GD()

cat("== test-01  Reproduce Song et al. (2020) ndvi_40 factor detector ==========\n")
data(ndvi_40, package = "GD")
dm <- c("equal", "natural", "quantile", "geometric", "sd"); di <- 3:7
d  <- ndvi_40
for (v in c("Tempchange", "Precipitation", "GDP", "Popdensity")) {
  od <- optidisc(stats::as.formula(paste("NDVIchange ~", v)),
                 data = ndvi_40, discmethod = dm, discitv = di)[[1]]
  d[[v]] <- cut(ndvi_40[[v]], unique(od$itv), include.lowest = TRUE)
}
f <- gd(NDVIchange ~ ., data = d)$Factor
q <- setNames(f$qv, f$variable)

# Expected values from the OPGD paper's NDVI case (GISci & RS 57:5, Fig 6/7):
# Precipitation and Climate zone are the two dominant drivers.
checks <- list(
  c("Precipitation dominates (Q > 0.80)", q["Precipitation"] > 0.80),
  c("Climate zone strong (Q > 0.80)",     q["Climatezone"]   > 0.80),
  c("Precipitation ranks above Climate zone", q["Precipitation"] > q["Climatezone"]),
  c("GDP is the weakest driver (Q < 0.15)", q["GDP"] < 0.15),
  c("every Q is a valid fraction in [0,1]", all(q >= 0 & q <= 1))
)
pass <- 0
for (c1 in checks) {
  ok <- isTRUE(as.logical(c1[[2]]))
  cat(sprintf("   %-45s %s\n", c1[[1]], if (ok) "pass" else "FAIL"))
  pass <- pass + ok
}
cat(sprintf("\n   Q: Precip=%.3f  Climate=%.3f  Temp=%.3f  Pop=%.3f  GDP=%.3f\n",
            q["Precipitation"], q["Climatezone"], q["Tempchange"],
            q["Popdensity"], q["GDP"]))
if (pass == length(checks))
  cat("   PASS — the template reproduces the published OPGD ranking\n") else
  stop(sprintf("test-01 FAILED (%d/%d)", pass, length(checks)))
