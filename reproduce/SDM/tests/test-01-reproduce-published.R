# =============================================================================
# test-01-reproduce-published.R — the analysis table is the published one
#
# The access, accessibility and delta values are the authors' own, so what has
# to be checked is that they are intact and that the model's defining identity
# holds on them: delta = accessibility - access, at every spatial scale.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

cat("== test-01  The published analysis table ================================\n")
pass <- 0; total <- 0
check <- function(label, cond) {
  total <<- total + 1; ok <- isTRUE(cond)
  cat(sprintf("   %-58s %s\n", label, if (ok) "pass" else "FAIL")); pass <<- pass + ok
}

d <- read_analysis()
check("8259 residential blocks, as published", nrow(d) == 8259)
check("nine SA3 regions", length(unique(d[[REGION_COL]])) == 9)
check("six walking times are present",
      all(vapply(WALK_TIMES, function(t)
        all(c(col_access(t), col_accessibility(t), col_delta(t)) %in% names(d)), logical(1))))

## -- the model identity -------------------------------------------------------
worst <- max(vapply(WALK_TIMES, function(t)
  max(abs(d[[col_delta(t)]] - (d[[col_accessibility(t)]] - d[[col_access(t)]]))), numeric(1)))
check(sprintf("delta = accessibility - access at every scale (max dev %.1e)", worst),
      worst < 1e-4)

## -- access and accessibility are standardised --------------------------------
for (t in c(WALK_TIMES[1], WALK_TIMES[length(WALK_TIMES)])) {
  m <- mean(d[[col_access(t)]]); s <- stats::sd(d[[col_access(t)]])
  check(sprintf("access at %2d min is standardised (mean %.2f, sd %.2f)", t, m, s),
        abs(m) < 0.05 && abs(s - 1) < 0.05)
}

## -- the twelve explanatory variables ----------------------------------------
check("six raw explanatory variables are present", all(VARS_RAW %in% names(d)))
check("six contextualised variables are present", all(VARS_CTX %in% names(d)))

## -- the geometry joins -------------------------------------------------------
if (has_pkg("sf")) {
  b <- sf::read_sf(file.path(PROJ_ROOT, BLOCKS_FILE))
  res <- b[b$MB_CATEGOR == CONSUMER_CAT, ]
  matched <- sum(d[[ID_COL]] %in% as.character(res[[ID_COL]]))
  check(sprintf("every analysis row matches a residential block (%d of %d)",
                matched, nrow(d)), matched == nrow(d))
  check("providers are present in the geometry",
        sum(b$MB_CATEGOR == PROVIDER_CAT) > 1000)
} else {
  cat("   (sf not installed; geometry checks skipped)\n")
}

cat(sprintf("\n   %d passed, %d failed\n", pass, total - pass))
if (pass == total)
  cat("   PASS - the published analysis table is intact\n") else
  stop("test-01 had failures")
