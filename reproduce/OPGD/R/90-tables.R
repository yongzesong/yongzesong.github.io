# =============================================================================
# 90-tables.R — LaTeX tables for the manuscript + the session record.
# Every number reaches the paper through these files; nothing is retyped.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Build manuscript tables")

tex_table <- function(df, file, caption, label, digits = 3) {
  fmt <- function(x) if (is.numeric(x)) formatC(round(x, digits), format = "f",
                                                digits = digits) else as.character(x)
  body <- apply(df, 1, function(r) paste(vapply(r, fmt, character(1)),
                                         collapse = " & "))
  con <- file.path(TAB_DIR, file)
  writeLines(c(
    "\\begin{table}[t]\\centering",
    paste0("\\caption{", caption, "}\\label{", label, "}"),
    paste0("\\begin{tabular}{", paste(rep("l", ncol(df)), collapse = ""), "}"),
    "\\hline",
    paste(paste(names(df), collapse = " & "), "\\\\"), "\\hline",
    paste(body, "\\\\"), "\\hline",
    "\\end{tabular}\\end{table}"), con)
  log_info("wrote tables/%s", file)
}

if (file.exists(F_DISC))
  tex_table(read_result(F_DISC), "table-optimal-discretization.tex",
            "Optimal discretisation parameters per continuous variable.",
            "tab:disc")
if (file.exists(F_FACTOR))
  tex_table(read_result(F_FACTOR), "table-factor.tex",
            "Factor detector: relative importance (Q) of explanatory variables.",
            "tab:factor")
if (file.exists(F_INTERACT)) {
  it <- read_result(F_INTERACT)[, c("var1", "var2", "qv12", "interaction")]
  tex_table(it, "table-interaction.tex",
            "Interaction detector: joint Q and interaction type per pair.",
            "tab:interact")
}

write_session_info()
log_info("wrote env/session-info.txt")
