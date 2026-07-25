# =============================================================================
# 90-tables.R — LaTeX tables for the manuscript + the session record.
# =============================================================================
if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))
log_head("Build manuscript tables")

tex_table <- function(df, file, caption, label, digits = 4) {
  fmt <- function(v) if (is.numeric(v))
    ifelse(is.na(v), "--", formatC(round(v, digits), format = "f", digits = digits)) else as.character(v)
  body <- apply(df, 1, function(r) paste(vapply(r, fmt, character(1)), collapse = " & "))
  writeLines(c("\\begin{table}[t]\\centering",
    paste0("\\caption{", caption, "}\\label{", label, "}"),
    paste0("\\begin{tabular}{", paste(rep("l", ncol(df)), collapse = ""), "}"), "\\hline",
    paste(paste(gsub("_", " ", names(df)), collapse = " & "), "\\\\"), "\\hline",
    paste(body, "\\\\"), "\\hline", "\\end{tabular}\\end{table}"), file.path(TAB_DIR, file))
  log_info("wrote tables/%s", file)
}

if (file.exists(F_DELTA))
  tex_table(read_result(F_DELTA), "table-delta-summary.tex",
            "The difference between accessibility and access by walking time.", "tab:delta")
if (file.exists(F_CLASS))
  tex_table(read_result(F_CLASS), "table-classification.tex",
            "Green city classification by walking time.", "tab:class")
if (file.exists(F_DRIVERS)) {
  pd <- read_result(F_DRIVERS)
  w <- stats::reshape(pd[, c("variable", "walk_minutes", "PD")], idvar = "variable",
                      timevar = "walk_minutes", direction = "wide")
  names(w) <- sub("^PD\\.", "", names(w))
  tex_table(w, "table-drivers-pd.tex",
            "Power of Determinant of each variable at each walking time.", "tab:pd")
}
if (file.exists(F_INTERACT)) {
  it <- read_result(F_INTERACT)
  tex_table(head(it[, c("variable1", "variable2", "PD1", "PD2", "PID", "interaction")], 15),
            "table-interactions.tex",
            "Power of Interaction Determinant, strongest fifteen pairs.", "tab:pid")
}
write_session_info(); log_info("wrote env/session-info.txt")
