# =============================================================================
# 40-drivers-gozh.R — what explains the difference
#
# The geographical detector measures how much of a variable's spatial variation
# a stratification explains (the Power of Determinant, PD). It needs strata, and
# the explanatory variables here are continuous, so a regression tree is used to
# cut each one into spatial zones — the GOZH approach: let the data choose the
# strata, then score them.
#
# Each variable appears twice: as its own value, and in a contextualised form
# carrying the local spatial pattern around each block. Comparing the two says
# whether what matters is the quantity at a place or its arrangement around it.
#
# Outputs: results/drivers-pd.csv        (PD per variable per walking time)
#          results/drivers-total-pd.csv   (all variables jointly)
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
need_pkg("rpart", "tree-based stratification")
need_pkg("GD", "the geographical detector")

if (!isTRUE(RUN_DRIVERS)) {
  log_info("RUN_DRIVERS is FALSE; skipping"); } else {

log_head("Step 4/5  Drivers of the difference")

d <- read_analysis()
vars <- explanatory_frame(d)
log_info("%d explanatory variables (%d raw, %d contextualised) plus region",
         ncol(vars) - 1, length(VARS_RAW), length(VARS_CTX))

#' Power of Determinant of one stratification of the response.
pd_of <- function(response, x) {
  dd <- data.frame(df = response, v = x)
  if (is.character(x) || is.factor(x)) {
    dd$strata <- as.character(x)
  } else {
    # Tree-based zoning: terminal nodes become the spatial strata.
    tr <- rpart::rpart(df ~ v, data = dd, minbucket = TREE_MINBUCKET)
    dd$strata <- as.character(as.numeric(tr$where))
  }
  if (length(unique(dd$strata)) < 2) return(c(qv = NA_real_, sig = NA_real_))
  g <- GD::gd(df ~ strata, dd)
  c(qv = as.numeric(g$Factor$qv), sig = as.numeric(g$Factor$sig))
}

## -- individual variables, at every walking time ------------------------------
rows <- list()
for (t in WALK_TIMES) {
  y <- d[[col_delta(t)]]
  for (j in seq_len(ncol(vars))) {
    r <- pd_of(y, vars[[j]])
    rows[[length(rows) + 1]] <- data.frame(
      walk_minutes = t, variable = names(vars)[j],
      type = if (names(vars)[j] == "Region") "region"
             else if (j <= 1 + length(VARS_RAW)) "raw" else "contextualised",
      PD = round(r[["qv"]], 4), sig = signif(r[["sig"]], 3), row.names = NULL)
  }
  top <- rows[(length(rows) - ncol(vars) + 1):length(rows)]
  top <- do.call(rbind, top)
  top <- top[order(-top$PD), ]
  log_info("%2d min: strongest is %s (PD = %.4f)", t, top$variable[1], top$PD[1])
}
pd <- do.call(rbind, rows)
pd <- pd[!is.na(pd$PD) & pd$PD > 0, ]
write_result(pd, F_DRIVERS)

## -- all variables jointly ----------------------------------------------------
# One tree over every variable at once: the ceiling on what this variable set
# can explain about the difference.
tot <- list()
for (t in WALK_TIMES) {
  dd <- data.frame(df = d[[col_delta(t)]], vars)
  tr <- rpart::rpart(df ~ ., data = dd, minbucket = TREE_MINBUCKET)
  dd$strata <- as.character(as.numeric(tr$where))
  g <- GD::gd(df ~ strata, dd)
  tot[[length(tot) + 1]] <- data.frame(
    walk_minutes = t, n_strata = length(unique(dd$strata)),
    PD = round(as.numeric(g$Factor$qv), 4), sig = signif(as.numeric(g$Factor$sig), 3))
  log_info("%2d min: all variables jointly explain PD = %.4f (%d strata)",
           t, tot[[length(tot)]]$PD, tot[[length(tot)]]$n_strata)
}
write_result(do.call(rbind, tot), F_TOTAL)

## -- does the arrangement matter more than the value? -------------------------
raw <- stats::aggregate(PD ~ walk_minutes, data = pd[pd$type == "raw", ], FUN = mean)
ctx <- stats::aggregate(PD ~ walk_minutes, data = pd[pd$type == "contextualised", ], FUN = mean)
cmp <- merge(raw, ctx, by = "walk_minutes", suffixes = c("_raw", "_ctx"))
for (i in seq_len(nrow(cmp)))
  log_info("%2d min: mean PD raw %.4f vs contextualised %.4f -> %s",
           cmp$walk_minutes[i], cmp$PD_raw[i], cmp$PD_ctx[i],
           ifelse(cmp$PD_ctx[i] > cmp$PD_raw[i], "arrangement matters more", "value matters more"))
}
