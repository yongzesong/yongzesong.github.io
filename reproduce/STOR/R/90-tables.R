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

# -- Entropy weights (the paper's Table 2) ------------------------------------
w <- read_result(F_WEIGHTS)
rows <- sprintf("%s & %s & %s & %.3f \\\\",
                gsub("_", "\\\\_", w$name), w$category,
                ifelse(w$dimension == "A", "$\\Gamma_A$ (quantity)",
                       "$\\Gamma_B$ (quality)"), w$weight)
tex_table(c("\\begin{tabular}{lllr}", "\\hline",
            "Variable & Category & Dimension & Entropy weight \\\\", "\\hline",
            rows, "\\hline", "\\end{tabular}"),
          file.path(TAB_DIR, "table-entropy-weights.tex"))

# -- DMU stage summary (the paper's Fig. 7a as a table) -----------------------
s <- read_result(F_STAGES)
agg <- do.call(rbind, lapply(split(s, s$stage), function(d)
  data.frame(stage = d$stage[1], blocks = nrow(d),
             share = nrow(d) / nrow(s),
             gamma_a = mean(d$gamma_a), gamma_b = mean(d$gamma_b),
             gamma = mean(d$gamma))))
rows <- sprintf("%s & %d & %.1f\\%% & %.3f & %.3f & %.3f \\\\",
                agg$stage, agg$blocks, 100 * agg$share,
                agg$gamma_a, agg$gamma_b, agg$gamma)
tex_table(c("\\begin{tabular}{lrrrrr}", "\\hline",
            "DMU stage & Blocks & Share & Mean $\\Gamma_A$ & Mean $\\Gamma_B$ & Mean $\\Gamma$ \\\\",
            "\\hline", rows, "\\hline", "\\end{tabular}"),
          file.path(TAB_DIR, "table-dmu-stages.tex"))

# -- Trade-off groups and strategies (the paper's Fig. 8b) --------------------
l <- read_result(F_LISA)
tab <- table(l$strategy)
rows <- sprintf("%s & %d & %.1f\\%% \\\\", names(tab), as.numeric(tab),
                100 * as.numeric(tab) / sum(tab))
tex_table(c("\\begin{tabular}{lrr}", "\\hline",
            "Development strategy & Blocks & Share \\\\", "\\hline",
            rows, "\\hline", "\\end{tabular}"),
          file.path(TAB_DIR, "table-tradeoff-strategies.tex"))

# -- Income contribution (the paper's Fig. 11a) -------------------------------
cc <- read_result(F_CONTRIB)
rows <- sprintf("%s & %s & %.1f\\%% \\\\", cc$model,
                gsub("Gamma_", "$\\\\Gamma_", paste0(cc$dimension, "$")),
                100 * cc$contribution)
tex_table(c("\\begin{tabular}{llr}", "\\hline",
            "Model & Dimension & Contribution to income \\\\", "\\hline",
            rows, "\\hline", "\\end{tabular}"),
          file.path(TAB_DIR, "table-income-contribution.tex"))
