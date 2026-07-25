# =============================================================================
# 90-tables.R — LaTeX tables for the manuscript + the session record.
# Every number reaches the paper through these files; nothing is retyped.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Build manuscript tables")

tex_table <- function(df, file, caption, label, digits = 4) {
  fmt <- function(x) if (is.numeric(x))
    formatC(round(x, digits), format = "f", digits = digits) else as.character(x)
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

if (file.exists(F_CV))
  tex_table(read_result(F_CV), "table-cross-validation.tex",
            "Cross-validation of the first and second dimensions of spatial association.",
            "tab:cv")
if (file.exists(F_SELPARAM)) {
  p <- read_result(F_SELPARAM)
  tex_table(p[, c("surface", "buffer_km", "quantile", "correlation")],
            "table-selected-variables.tex",
            "Selected second-dimension variables with their searching range $b$ and quantile $\\tau$.",
            "tab:selected", digits = 3)
}
if (file.exists(F_BUFFER))
  tex_table(read_result(F_BUFFER), "table-buffer-sensitivity.tex",
            "Predictive performance by searching range $b$.", "tab:buffer")

write_session_info()
log_info("wrote env/session-info.txt")
