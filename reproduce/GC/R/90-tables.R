# =============================================================================
# 90-tables.R — LaTeX tables for the manuscript + the session record.
# Every number reaches the paper through these files; nothing is retyped.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Build manuscript tables")

tex_table <- function(df, file, caption, label, digits = 4) {
  fmt <- function(v) if (is.numeric(v))
    ifelse(is.na(v), "--", formatC(round(v, digits), format = "f", digits = digits)) else as.character(v)
  body <- apply(df, 1, function(r) paste(vapply(r, fmt, character(1)), collapse = " & "))
  writeLines(c(
    "\\begin{table}[t]\\centering",
    paste0("\\caption{", caption, "}\\label{", label, "}"),
    paste0("\\begin{tabular}{", paste(rep("l", ncol(df)), collapse = ""), "}"),
    "\\hline",
    paste(paste(gsub("_", " ", names(df)), collapse = " & "), "\\\\"), "\\hline",
    paste(body, "\\\\"), "\\hline",
    "\\end{tabular}\\end{table}"), file.path(TAB_DIR, file))
  log_info("wrote tables/%s", file)
}

if (file.exists(F_BASE))
  tex_table(read_result(F_BASE), "table-baseline-models.tex",
            "Performance of the baseline models.", "tab:baseline")
if (file.exists(F_EXPLAIN))
  tex_table(read_result(F_EXPLAIN), "table-error-explanation.tex",
            "Contribution of geocomplexity in explaining spatial errors.", "tab:explain")
if (file.exists(F_IMPROVE))
  tex_table(read_result(F_IMPROVE), "table-model-improvement.tex",
            "Model performance with and without geocomplexity.", "tab:improve")
if (file.exists(F_GCMETHOD))
  tex_table(read_result(F_GCMETHOD), "table-gc-methods.tex",
            "Geocomplexity by calculation method.", "tab:gcmethod")

write_session_info()
log_info("wrote env/session-info.txt")
