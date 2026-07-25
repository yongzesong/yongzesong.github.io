# =============================================================================
# 20-optimal-range.R — Ripley's K and Besag's L give the local range
# =============================================================================
# The first step of LPA. Every later step depends on the single number this
# script produces, so it also records how sensitive that number is to the edge
# correction — which turns out to be the choice that matters most.
#
# Writes results/ripley-k-curve.csv and results/optimal-range.csv.
# =============================================================================

log_head("Step 20 — optimal local range")

d <- readRDS(F_POINTS)

# -- The curve the paper plots ------------------------------------------------
main <- ripley_L(d, correction = K_CORRECTION, window = K_WINDOW)
log_info("L peaks at r = %.2f km (correction '%s', %s window), L_max = %.2f",
         main$r_opt, K_CORRECTION, K_WINDOW, main$L_max)

# How sharply that peak is defined. A flat top means several radii read the data
# equally well, and quoting r_opt to two decimals would overstate the evidence.
ok  <- trim_collapse(main$r, main$L)
top <- main$r[ok & main$L > main$L_max - 5]
log_info("L is within 5 of its maximum from %.0f to %.0f km — the peak is a plateau %0.f km wide",
         min(top), max(top), diff(range(top)))
log_info("the border correction stops being reliable beyond %.0f km", max(main$r[ok]))

# Thin the curve before writing it: 120,000 rows of a smooth function help
# nobody, and the figure only needs a kilometre-scale grid.
keep <- seq(1, length(main$r), by = max(1, round(1 / K_R_STEP)))
curve <- data.frame(r = main$r[keep], K_observed = main$K[keep],
                    K_expected = main$theo[keep], L = main$L[keep])
write_result(curve, F_KCURVE)

# -- How much the edge correction moves it ------------------------------------
# Without a correction, pairs are missed near the boundary, K is biased down and
# the peak lands at roughly half the distance. Reporting the alternatives keeps
# the choice visible instead of buried in a default.
alts <- do.call(rbind, lapply(K_CORRECTIONS_COMPARED, function(cc) {
  fit <- try(ripley_L(d, correction = cc, window = K_WINDOW), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  data.frame(correction = cc, window = K_WINDOW,
             r_opt_km = fit$r_opt, L_max = fit$L_max, stringsAsFactors = FALSE)
}))
hull <- try(ripley_L(d, correction = K_CORRECTION, window = "hull"), silent = TRUE)
if (!inherits(hull, "try-error"))
  alts <- rbind(alts, data.frame(correction = K_CORRECTION, window = "hull",
                                 r_opt_km = hull$r_opt, L_max = hull$L_max))

alts$published_km <- 707.29
alts$difference_km <- alts$r_opt_km - alts$published_km
alts$selected <- alts$correction == K_CORRECTION & alts$window == K_WINDOW
alts$plateau_from_km <- ifelse(alts$selected, min(top), NA_real_)
alts$plateau_to_km   <- ifelse(alts$selected, max(top), NA_real_)
write_result(alts, F_RANGE)
print(alts, row.names = FALSE, digits = 5)

R_OPT <- if (!is.null(RADIUS_OVERRIDE)) RADIUS_OVERRIDE else main$r_opt
log_info("local range carried forward: %.2f km (published 707.29 km, %+.2f km)",
         R_OPT, R_OPT - 707.29)
saveRDS(R_OPT, file.path(DERIVED, "r-opt.rds"))

# -- How many neighbours that radius buys -------------------------------------
nb <- vapply(seq_len(nrow(d)), function(i) length(neighbours_within(d, i, R_OPT)),
             integer(1))
log_info("neighbourhood size: median %.0f, range %d-%d; %d locations below the minimum of %d",
         stats::median(nb), min(nb), max(nb), sum(nb < MIN_LOCAL_N), MIN_LOCAL_N)
saveRDS(nb, file.path(DERIVED, "neighbour-counts.rds"))
