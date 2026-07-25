# =============================================================================
# 40-reproduce-published.R — rebuild Table 2, then test the refit against it
# =============================================================================
# Two questions, kept apart on purpose.
#
#   (1) Do the authors' own numbers follow from the authors' own output file?
#       This is arithmetic, and it either matches to the printed decimal or it
#       does not.
#   (2) Does an independent implementation of LPA land on the same estimates?
#       This is reproduction in the harder sense, and the answer is partial.
#
# Writes results/table2-published-reproduction.csv and
#        results/recomputed-vs-published.csv.
# =============================================================================

log_head("Step 40 — reproduce the published values")

d <- readRDS(F_POINTS)

# The values printed in Table 2 of the paper, transcribed for comparison.
PUBLISHED <- data.frame(
  published = PATHS$published,
  mean = c(-0.370, 0.798, -0.763, 0.773, -0.269, 0.335, 0.427),
  min  = c(-0.967, -0.605, -0.987, 0.004, -0.998, -0.994, -0.969),
  max  = c(0.747, 0.999, 0.430, 1.000, 0.774, 0.998, 0.997),
  pct  = c(94.39, 98.29, 96.80, 98.21, 70.07, 63.52, 62.35),
  global = c(-0.332, 0.858, -0.895, 0.995, -0.528, 0.267, 0.677),
  stringsAsFactors = FALSE
)

# -- (1) Table 2 from the authors' output file --------------------------------
# Two conventions have to be right for the numbers to appear. The mean, minimum
# and maximum are taken over lambda inside [-1, 1] — the filter the authors'
# plotting script applies — while the significance share is taken over the
# locations that returned a complete set of p-values, not over all locations.
tab <- do.call(rbind, lapply(seq_len(nrow(PATHS)), function(j) {
  cc <- PATHS$published[j]
  s  <- summarise_lambda(d[[cc]], d[[paste0("sig.", cc)]])
  data.frame(lambda = PATHS$lambda[j], path = PATHS$label[j],
             kind = PATHS$kind[j], s, stringsAsFactors = FALSE)
}))
# PATHS, PUBLISHED and tab are all in lambda_1..lambda_7 order, so the paper's
# figures line up row by row.
tab$mean_paper   <- PUBLISHED$mean
tab$min_paper    <- PUBLISHED$min
tab$max_paper    <- PUBLISHED$max
tab$pct_paper    <- PUBLISHED$pct
tab$global_paper <- PUBLISHED$global

tab$d_mean <- round(tab$mean, 3) - tab$mean_paper
tab$d_min  <- round(tab$min, 3)  - tab$min_paper
tab$d_max  <- round(tab$max, 3)  - tab$max_paper
tab$d_pct  <- round(tab$pct_significant, 2) - tab$pct_paper
tab$exact  <- abs(tab$d_mean) < 1e-9 & abs(tab$d_min) < 1e-9 &
              abs(tab$d_max) < 1e-9 & abs(tab$d_pct) < 1e-9

write_result(tab, F_TABLE2)

cat("\n")
cat(sprintf("%-26s %-22s %-22s %s\n", "Path", "recomputed", "Table 2", "match"))
for (j in seq_len(nrow(tab))) {
  cat(sprintf("%-26s %6.3f [%6.3f,%6.3f] %6.3f [%6.3f,%6.3f] %s\n",
      tab$path[j], round(tab$mean[j], 3), round(tab$min[j], 3), round(tab$max[j], 3),
      tab$mean_paper[j], tab$min_paper[j], tab$max_paper[j],
      ifelse(tab$exact[j], "yes", "NO")))
  cat(sprintf("%-26s %21s %21s\n", "",
      sprintf("%.2f %% sig", round(tab$pct_significant[j], 2)),
      sprintf("%.2f %% sig", tab$pct_paper[j])))
}
log_info("%d of %d rows reproduce every printed figure exactly",
         sum(tab$exact), nrow(tab))

# -- (2) The independent refit against the published estimates ----------------
if (!file.exists(F_LAMBDA)) {
  log_warn("no recomputed lambda yet — run step 30 first")
} else {
  rec <- read_result(F_LAMBDA)
  cmp <- do.call(rbind, lapply(seq_len(nrow(PATHS)), function(j) {
    cc <- PATHS$published[j]
    a <- rec[[cc]]; b <- d[[cc]][rec$id]
    ok <- is.finite(a) & is.finite(b) &
          abs(a) <= LAMBDA_PLOT_RANGE[2] & abs(b) <= LAMBDA_PLOT_RANGE[2]
    data.frame(
      lambda = PATHS$lambda[j], path = PATHS$label[j], kind = PATHS$kind[j],
      n_compared = sum(ok),
      correlation = if (sum(ok) > 3) stats::cor(a[ok], b[ok]) else NA_real_,
      sign_agreement = 100 * mean(sign(a[ok]) == sign(b[ok])),
      mean_recomputed = mean(a[ok]), mean_published = mean(b[ok]),
      median_abs_difference = stats::median(abs(a[ok] - b[ok])),
      stringsAsFactors = FALSE)
  }))
  write_result(cmp, F_AGREE)
  cat("\n")
  print(cmp[, c("lambda", "path", "n_compared", "correlation",
                "sign_agreement", "mean_recomputed", "mean_published")],
        row.names = FALSE, digits = 3)
  log_info("sign agreement %.0f-%.0f %%, correlation %.2f-%.2f across the seven paths",
           min(cmp$sign_agreement, na.rm = TRUE), max(cmp$sign_agreement, na.rm = TRUE),
           min(cmp$correlation, na.rm = TRUE), max(cmp$correlation, na.rm = TRUE))
  log_info("the refit reproduces the pattern of the published surfaces, not the individual estimates")
}
