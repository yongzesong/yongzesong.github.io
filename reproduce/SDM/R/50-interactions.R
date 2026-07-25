# =============================================================================
# 50-interactions.R — do pairs of variables explain more together?
#
# The interaction detector overlays two stratifications and scores the combined
# zones. If the pair explains more than either alone, the two act together; the
# five standard classes distinguish how.
#
# Output: results/interaction-pid.csv
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

if (!isTRUE(RUN_INTERACTIONS)) {
  log_info("RUN_INTERACTIONS is FALSE; skipping"); } else {
need_pkg("rpart", "tree-based stratification")
need_pkg("GD", "the geographical detector")

log_head("Step 5/5  Interactions between variables")

d <- read_analysis()
vars <- explanatory_frame(d)
t <- INTERACTION_TIME
y <- d[[col_delta(t)]]
log_info("interaction detector at %d minutes", t)

#' Strata for one variable: its own categories, or tree-based zones.
strata_of <- function(x) {
  if (is.character(x) || is.factor(x)) return(as.character(x))
  dd <- data.frame(df = y, v = x)
  tr <- rpart::rpart(df ~ v, data = dd, minbucket = TREE_MINBUCKET)
  as.character(as.numeric(tr$where))
}
pd_strata <- function(s) {
  if (length(unique(s)) < 2) return(NA_real_)
  as.numeric(GD::gd(df ~ strata, data.frame(df = y, strata = s))$Factor$qv)
}

S <- lapply(vars, strata_of)
single <- vapply(S, pd_strata, numeric(1))

#' The five interaction classes of the geographical detector.
classify <- function(q1, q2, q12) {
  lo <- min(q1, q2); hi <- max(q1, q2)
  if (q12 < lo) "Weaken, nonlinear"
  else if (q12 < hi) "Weaken, uni-"
  else if (isTRUE(all.equal(q12, q1 + q2))) "Independent"
  else if (q12 < q1 + q2) "Enhance, bi-"
  else "Enhance, nonlinear"
}

nm <- names(vars)
rows <- list()
for (i in seq_along(nm)) for (j in seq_along(nm)) {
  if (j <= i) next
  q12 <- pd_strata(paste(S[[i]], S[[j]], sep = "_"))
  if (is.na(q12) || is.na(single[i]) || is.na(single[j])) next
  rows[[length(rows) + 1]] <- data.frame(
    walk_minutes = t, variable1 = nm[i], variable2 = nm[j],
    PD1 = round(single[i], 4), PD2 = round(single[j], 4),
    PID = round(q12, 4), interaction = classify(single[i], single[j], q12),
    row.names = NULL)
}
it <- do.call(rbind, rows)
it <- it[order(-it$PID), ]
write_result(it, F_INTERACT)

log_info("strongest pair: %s x %s -> PID = %.4f (%s)",
         it$variable1[1], it$variable2[1], it$PID[1], it$interaction[1])
log_info("singles reach at most PD = %.4f, so the pair adds %.1f%%",
         max(single, na.rm = TRUE), (it$PID[1] / max(single, na.rm = TRUE) - 1) * 100)
tb <- table(it$interaction)
log_info("interaction types: %s", paste(sprintf("%s %d", names(tb), as.integer(tb)), collapse = ", "))
}
