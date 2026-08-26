# =============================================================================
# 90-tables.R — LaTeX tables for the manuscript
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("90  Tables")

tex_table <- function(lines, path) {
  writeLines(lines, path)
  log_info("wrote %s", sub(paste0(PROJ_ROOT, "/"), "", path))
}

# -- Simulation, the paper's Table 1 -----------------------------------------
s <- read_result(F_SIM_RESULTS)
rows <- unlist(lapply(c("normal", "skewed", "long-tail"), function(sc) {
  g  <- s[s$scenario == sc, ]
  ok <- g[g$model == "OK", ]; sk <- g[g$model == "SRK", ]
  sprintf("%s & %.3f & %.3f & %+.3f & %.3f & %.3f & %+.3f & %.3f & %.3f & %+.3f \\\\",
          sc, ok$R2, sk$R2, sk$R2 - ok$R2, ok$RMSE, sk$RMSE, sk$RMSE - ok$RMSE,
          ok$MAE, sk$MAE, sk$MAE - ok$MAE)
}))
tex_table(c("\\begin{tabular}{lrrrrrrrrr}", "\\hline",
            "& \\multicolumn{3}{c}{$R^2$} & \\multicolumn{3}{c}{RMSE} & \\multicolumn{3}{c}{MAE} \\\\",
            "Simulated field & OK & SRK & $\\Delta$ & OK & SRK & $\\Delta$ & OK & SRK & $\\Delta$ \\\\",
            "\\hline", rows, "\\hline", "\\end{tabular}"),
          file.path(TAB_DIR, "table-simulation.tex"))

# -- Model comparison, the paper's Table 2 -----------------------------------
cv <- read_result(F_CV_SUMMARY)
ord <- c("SRK", "OK", "IDW", "LM", "RF", "RFK")
for (elem in names(ELEMENTS)) {
  g <- cv[cv$element == elem, ]
  g <- g[match(ord, g$model), ]
  rows <- unlist(lapply(seq_len(nrow(g)), function(i) {
    r <- g[i, ]
    line <- sprintf("%s & %.3f & %.2f & %.2f \\\\", r$model, r$R2, r$RMSE, r$MAE)
    if (r$model == "SRK") return(line)
    c(line, sprintf("\\quad $\\Delta$ vs SRK & %+.3f (%.1f\\%%) & %+.2f (%.1f\\%%) & %+.2f (%.1f\\%%) \\\\",
                    r$dR2_vs_SRK, 100 * r$dR2_vs_SRK / abs(r$R2),
                    -r$dRMSE_vs_SRK, -100 * r$dRMSE_vs_SRK / r$RMSE,
                    -r$dMAE_vs_SRK, -100 * r$dMAE_vs_SRK / r$MAE))
  }))
  tex_table(c("\\begin{tabular}{lrrr}", "\\hline",
              sprintf("Model & $R^2$ & RMSE (%s) & MAE (%s) \\\\",
                      ELEMENTS[[elem]]$unit, ELEMENTS[[elem]]$unit),
              "\\hline", rows, "\\hline", "\\end{tabular}"),
            file.path(TAB_DIR, sprintf("table-model-comparison-%s.tex", elem)))
}

# -- Covariate screening ------------------------------------------------------
sc <- read_result(F_SCREEN)
rows <- sprintf("%s & %s & %+.3f & %s & %s \\\\", sc$element, sc$covariate,
                sc$rho, ifelse(sc$p < 0.001, "$<0.001$", sprintf("%.3f", sc$p)),
                ifelse(sc$significant, "kept", "dropped"))
tex_table(c("\\begin{tabular}{llrrl}", "\\hline",
            "Element & Covariate & Spearman $\\rho$ & $p$ & Screen \\\\",
            "\\hline", rows, "\\hline", "\\end{tabular}"),
          file.path(TAB_DIR, "table-covariate-screening.tex"))

# -- Singularity feature diagnostics ------------------------------------------
sv <- read_result(F_SV_DIAG)
rows <- sprintf("%s & sv(%s) & %.2f & %.2f & %.2f & %+.3f & %s \\\\",
                sv$element, sv$covariate, sv$sv_mean, sv$sv_sd,
                100 * sv$share_below_2, sv$cor_with_response,
                ifelse(sv$retained, "retained", "dropped"))
tex_table(c("\\begin{tabular}{llrrrrl}", "\\hline",
            "Element & Feature & Mean $\\alpha$ & SD & \\% $\\alpha<2$ & $r$ with response & SD filter \\\\",
            "\\hline", rows, "\\hline", "\\end{tabular}"),
          file.path(TAB_DIR, "table-singularity-features.tex"))

# -- Seed stability -----------------------------------------------------------
if (file.exists(file.path(RES_DIR, "seed-stability-spread.csv"))) {
  sp <- read_result(file.path(RES_DIR, "seed-stability-spread.csv"))
  rows <- sprintf("%s & %s & %.4f & %.4f & %.4f & %.4f \\\\", sp$element, sp$model,
                  sp$R2_mean, sp$R2_sd, sp$R2_min, sp$R2_max)
  tex_table(c("\\begin{tabular}{llrrrr}", "\\hline",
              sprintf("Element & Model & mean $R^2$ & SD & min & max \\\\ \\multicolumn{6}{l}{\\footnotesize over %d random-forest seeds, folds held fixed} \\\\",
                      SEED_REPEATS),
              "\\hline", rows, "\\hline", "\\end{tabular}"),
            file.path(TAB_DIR, "table-seed-stability.tex"))
}

write_session_info()
log_info("wrote env/session-info.txt")
