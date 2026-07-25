# =============================================================================
# 90-tables.R — LaTeX tables, ready to paste into a manuscript
# =============================================================================

log_head("Step 90 — tables")

esc <- function(x) gsub("_", "\\\\_", gsub("~", "$\\\\sim$", gsub("=~", "$=\\\\sim$", x)))

#' Format a number for a LaTeX table.
#'
#' Math mode, so that a negative sign is typeset as a minus rather than as the
#' hyphen a bare "-0.370" produces. Everything here ends up in a manuscript.
num <- function(x, digits = 3, signed = FALSE) {
  fmt <- paste0("%", if (signed) "+" else "", ".", digits, "f")
  ifelse(is.finite(x), paste0("$", sprintf(fmt, x), "$"), "--")
}

write_tex <- function(lines, file) {
  writeLines(lines, file.path(TAB_DIR, file))
  log_info("wrote tables/%s", file)
}

# -- Table 1: the optimal local range and how it depends on the correction ----
if (file.exists(F_RANGE)) {
  r <- read_result(F_RANGE)
  # Bold the selected row cell by cell: a \textbf{} group cannot span the
  # alignment tabs of a tabular row.
  bf <- function(x, sel) ifelse(sel, paste0("\\textbf{", x, "}"), x)
  body <- sprintf("%s & %s & %s & %s & %s \\\\",
                  bf(r$correction, r$selected), bf(r$window, r$selected),
                  bf(num(r$r_opt_km, 2), r$selected),
                  bf(num(r$L_max, 2), r$selected),
                  bf(num(r$difference_km, 2, signed = TRUE), r$selected))
  write_tex(c(
    "\\begin{table}[!ht]", "\\centering",
    "\\caption{Optimal local range from Ripley's K under four estimation choices. The selected row is the one that reproduces the published range of 707.29 km.}",
    "\\label{tab:local-range}",
    "\\begin{tabular}{llrrr}", "\\hline",
    "Edge correction & Window & $r_{opt}$ (km) & $L_{max}$ & Difference (km) \\\\",
    "\\hline", body, "\\hline", "\\end{tabular}", "\\end{table}"),
    "table-local-range.tex")
}

# -- Table 2: the paper's validation table, rebuilt -------------------------
if (file.exists(F_TABLE2)) {
  t2 <- read_result(F_TABLE2)
  meas <- t2[t2$kind == "Measurement", ]; stru <- t2[t2$kind == "Structural", ]
  # Every global coefficient in the article carries two stars.
  row <- function(s) sprintf("%s & %s [%s, %s] & %s \\%% & %s** \\\\",
                             esc(s$path), num(s$mean), num(s$min), num(s$max),
                             num(s$pct_significant, 2), num(s$global_paper))
  write_tex(c(
    "\\begin{table}[!ht]", "\\centering",
    "\\caption{Local pathways of spatial association identified by the LPA model, against the global structural equation estimates. Summaries cover the locations whose coefficient falls inside $[-1, 1]$; the significance share covers the locations returning a complete set of $p$-values.}",
    "\\label{tab:lpa-validation}",
    "\\begin{tabular}{lccc}", "\\hline",
    "Paths & Local $\\lambda$ mean [min, max] & Significant ($p<0.05$) & Global $\\lambda$ \\\\",
    "\\hline", "\\multicolumn{4}{l}{\\textit{Variable relationship}} \\\\",
    unlist(lapply(seq_len(nrow(meas)), function(i) row(meas[i, ]))),
    "\\multicolumn{4}{l}{\\textit{Latent variable relationships}} \\\\",
    unlist(lapply(seq_len(nrow(stru)), function(i) row(stru[i, ]))),
    "\\hline",
    "\\multicolumn{4}{l}{\\footnotesize $=\\sim$: regression between an observed and a latent variable; $\\sim$: between two latent variables.} \\\\",
    "\\multicolumn{4}{l}{\\footnotesize Significance of the global estimates: ** $p<0.01$, * $p<0.05$.} \\\\",
    "\\end{tabular}", "\\end{table}"),
    "table-lpa-validation.tex")
}

# -- Table 3: the independent refit -----------------------------------------
if (file.exists(F_AGREE)) {
  a <- read_result(F_AGREE)
  write_tex(c(
    "\\begin{table}[!ht]", "\\centering",
    "\\caption{An independent implementation of the LPA model against the published estimates. Agreement is computed at the locations where both estimates fall inside $[-1, 1]$.}",
    "\\label{tab:lpa-refit}",
    "\\begin{tabular}{lrrrr}", "\\hline",
    "Path & $n$ & Correlation & Sign agreement & Median $|\\Delta\\lambda|$ \\\\",
    "\\hline",
    sprintf("%s & %d & %s & %s \\%% & %s \\\\", esc(a$path), a$n_compared,
            num(a$correlation), num(a$sign_agreement, 1),
            num(a$median_abs_difference)),
    "\\hline", "\\end{tabular}", "\\end{table}"),
    "table-refit-agreement.tex")
}

# -- Table 4: radius sensitivity --------------------------------------------
if (file.exists(F_SENS)) {
  s <- read_result(F_SENS)
  w <- stats::reshape(s[, c("radius_km", "lambda", "mean")],
                      idvar = "radius_km", timevar = "lambda", direction = "wide")
  names(w) <- sub("^mean\\.", "", names(w))
  hdr <- paste(c("Radius (km)", sprintf("$\\lambda_%d$", 1:7)), collapse = " & ")
  # The chosen radius carries decimals; the comparison radii are round numbers.
  fmt_r <- function(x) if (abs(x - round(x)) < 1e-8) num(x, 0) else num(x, 2)
  body <- apply(w, 1, function(r)
    sprintf("%s & %s \\\\", fmt_r(as.numeric(r[1])),
            paste(num(as.numeric(r[-1])), collapse = " & ")))
  write_tex(c(
    "\\begin{table}[!ht]", "\\centering",
    "\\caption{Mean local path coefficient at six local ranges, over a fixed random subsample of locations.}",
    "\\label{tab:radius-sensitivity}",
    paste0("\\begin{tabular}{l", strrep("r", 7), "}"), "\\hline",
    paste0(hdr, " \\\\"), "\\hline", body, "\\hline",
    "\\end{tabular}", "\\end{table}"),
    "table-radius-sensitivity.tex")
}
