# =============================================================================
# 20-select-variables.R — characterise the geographical configuration (Eq. 1)
#
# The paper's Sect. 2.2.1 defines a two-stage screen: keep the covariates
# significantly correlated with the response, then drop the most collinear one
# repeatedly until every VIF is below 4. What survives *is* the geographical
# configuration e = {e_i} that every similarity in Eqs. 2-4 is computed from.
#
# Outputs: data/derived/selected-vars.rds
#          results/variable-selection.csv   (paper Table 2 shape)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Step 2/5  Select variables")

dat <- readRDS(D_SAMPLES)
d <- dat$samples
y <- d[[RESPONSE_LOG]]

# -- stage 1: correlation with the log-transformed response --------------------
cors <- do.call(rbind, lapply(CANDIDATES, function(v) {
  ct <- stats::cor.test(y, d[[v]])
  data.frame(variable = v, r = unname(ct$estimate), p = ct$p.value,
             stringsAsFactors = FALSE)
}))
cors <- cors[order(-abs(cors$r)), ]
cors$significant <- cors$p < COR_ALPHA

log_info("correlation with %s (|r| descending):", RESPONSE_LOG)
for (i in seq_len(nrow(cors)))
  log_info("  %-10s r = %+.3f  p = %.3g%s", cors$variable[i], cors$r[i], cors$p[i],
           if (cors$significant[i]) "" else "   (dropped)")

sig <- cors$variable[cors$significant]
if (length(sig) < 2) stop("fewer than two covariates survive the correlation screen")

# -- stage 2: iterative VIF pruning at the paper's threshold of 4 --------------
pr <- vif_prune(d, sig, VIF_THRESHOLD)
selected <- pr$keep
log_info("VIF screen kept %d of %d covariates (max VIF %.3f)",
         length(selected), length(sig), max(pr$vif))
if (length(pr$dropped))
  log_info("dropped for collinearity: %s", paste(pr$dropped, collapse = ", "))

# car::vif() is the reference implementation; when it happens to be installed,
# confirm the base-R VIFs in 01-helpers.R agree with it.
if (has_pkg("car")) {
  f_all <- stats::as.formula(paste(RESPONSE_LOG, "~", paste(selected, collapse = " + ")))
  ref <- car::vif(stats::lm(f_all, data = d))
  log_info("car::vif agreement: max |difference| = %.2e",
           max(abs(ref[selected] - pr$vif[selected])))
} else {
  log_info("car not installed; VIFs come from the base-R 1/(1-R2) identity")
}

# -- does the data-driven set match the one the vignette uses? -----------------
same <- setequal(selected, VIGNETTE_VARS)
log_info("selected: %s", paste(selected, collapse = ", "))
log_info("vignette: %s", paste(VIGNETTE_VARS, collapse = ", "))
if (same) {
  log_info("the two sets are identical")
} else {
  log_warn("sets differ — only in selection: %s | only in vignette: %s",
           paste(setdiff(selected, VIGNETTE_VARS), collapse = ", "),
           paste(setdiff(VIGNETTE_VARS, selected), collapse = ", "))
}

# -- the table the tutorial cites ---------------------------------------------
final_vif <- rep(NA_real_, nrow(cors))
names(final_vif) <- cors$variable
final_vif[selected] <- pr$vif[selected]

varsel <- data.frame(
  variable    = cors$variable,
  r           = round(cors$r, 4),
  p           = signif(cors$p, 4),
  significant = cors$significant,
  vif         = round(final_vif, 4),
  selected    = cors$variable %in% selected,
  in_vignette = cors$variable %in% VIGNETTE_VARS,
  stringsAsFactors = FALSE)
write_result(varsel, F_VARSEL)

saveRDS(list(selected = selected, dropped = pr$dropped, vif = pr$vif,
             correlations = cors, matches_vignette = same), D_SELECTED)
log_info("wrote data/derived/selected-vars.rds")
