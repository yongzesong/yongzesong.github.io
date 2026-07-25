# =============================================================================
# 30-delta.R — the Spatial Delta Model itself
#
# delta = beta - alpha, on standardised scales. The sign is the whole point:
#   delta > 0  accessibility exceeds access — the greenspace that is there is
#              easy to reach and use; a well-designed environment
#   delta = 0  the two agree — greenspace is integrated but its usability is
#              not adding anything
#   delta < 0  accessibility falls short of access — greenspace is present but
#              hard to reach or use
#
# Outputs: results/delta-summary.csv             (delta by walking time)
#          results/green-city-classification.csv (blocks by delta class)
#          results/delta-by-region.csv           (which regions do well)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Step 3/5  The difference between accessibility and access")

d <- read_analysis()

## -- delta across the walking times -------------------------------------------
rows <- lapply(WALK_TIMES, function(t) {
  a <- d[[col_access(t)]]; b <- d[[col_accessibility(t)]]; dl <- d[[col_delta(t)]]
  data.frame(walk_minutes = t, distance_m = WALK_DISTANCES[match(t, WALK_TIMES)],
             mean_delta = round(mean(dl), 4), sd_delta = round(stats::sd(dl), 4),
             min_delta = round(min(dl), 4), max_delta = round(max(dl), 4),
             share_positive = round(mean(dl > 0), 4),
             share_negative = round(mean(dl < 0), 4),
             cor_access_accessibility = round(stats::cor(a, b), 4))
})
ds <- do.call(rbind, rows)
write_result(ds, F_DELTA)
for (i in seq_len(nrow(ds)))
  log_info("%2d min: mean delta %+.3f, %.1f%% of blocks positive, cor(alpha,beta) = %+.3f",
           ds$walk_minutes[i], ds$mean_delta[i], 100 * ds$share_positive[i],
           ds$cor_access_accessibility[i])

## -- the green city classification --------------------------------------------
# Delta is a continuous measure, so "equal" is a band around zero rather than a
# single value. The band is one standard deviation of delta divided by ten,
# which keeps the classification comparable across walking times.
cls <- lapply(WALK_TIMES, function(t) {
  dl <- d[[col_delta(t)]]
  eps <- stats::sd(dl) / 10
  k <- ifelse(dl > eps, "positive", ifelse(dl < -eps, "negative", "zero"))
  data.frame(walk_minutes = t, class = c("positive", "zero", "negative"),
             n = as.integer(table(factor(k, levels = c("positive", "zero", "negative")))),
             share = round(as.numeric(prop.table(table(factor(k,
                       levels = c("positive", "zero", "negative"))))), 4))
})
cls <- do.call(rbind, cls)
write_result(cls, F_CLASS)
p5 <- cls[cls$walk_minutes == 5, ]
log_info("at 5 min: %.1f%% positive, %.1f%% near zero, %.1f%% negative",
         100 * p5$share[p5$class == "positive"], 100 * p5$share[p5$class == "zero"],
         100 * p5$share[p5$class == "negative"])

## -- which regions do well, and at which scale --------------------------------
reg <- do.call(rbind, lapply(WALK_TIMES, function(t) {
  agg <- stats::aggregate(d[[col_delta(t)]], by = list(region = d[[REGION_COL]]), FUN = mean)
  data.frame(walk_minutes = t, region = agg$region, mean_delta = round(agg$x, 4))
}))
write_result(reg, F_REGION)
r5 <- reg[reg$walk_minutes == 5, ]
r5 <- r5[order(-r5$mean_delta), ]
log_info("at 5 min, highest delta: %s (%+.3f); lowest: %s (%+.3f)",
         r5$region[1], r5$mean_delta[1],
         r5$region[nrow(r5)], r5$mean_delta[nrow(r5)])
