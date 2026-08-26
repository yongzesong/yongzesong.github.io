# =============================================================================
# 90-tables.R — LaTeX versions of the three tables the tutorial cites,
#               plus the session record. Nothing is retyped by hand.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_head("Build tables")

#' Write a data frame as a booktabs-free LaTeX table.
#' `digits` may be a single number or one per column; NA cells print as "--"
#' and logicals as yes/no, which is what the selection table needs. Columns
#' named in `sci` are written in scientific notation, because rounding a
#' p-value of 5e-35 to three decimals would print a misleading 0.000.
tex_table <- function(df, file, caption, label, digits = 3, align = NULL,
                      header = names(df), sci = character(0)) {
  digits <- rep_len(digits, ncol(df))
  fmt_col <- function(v, dg, nm) {
    if (is.logical(v)) return(ifelse(is.na(v), "--", ifelse(v, "yes", "no")))
    if (is.numeric(v) && nm %in% sci)
      return(ifelse(is.na(v), "--",
                    sub("e([+-])0*([0-9]+)$", "$\\\\times 10^{\\1\\2}$",
                        formatC(v, format = "e", digits = 1))))
    if (is.numeric(v)) return(ifelse(is.na(v), "--",
      formatC(round(v, dg), format = "f", digits = dg)))
    gsub("([&%_#])", "\\\\\\1", as.character(v))
  }
  cells <- as.data.frame(Map(fmt_col, df, digits, names(df)),
                         stringsAsFactors = FALSE)
  if (is.null(align))
    align <- paste0("l", strrep("r", ncol(df) - 1))
  body <- apply(cells, 1, paste, collapse = " & ")
  writeLines(c(
    "\\begin{table}[t]\\centering",
    paste0("\\caption{", caption, "}\\label{", label, "}"),
    paste0("\\begin{tabular}{", align, "}"),
    "\\hline",
    paste(paste(gsub("_", " ", header), collapse = " & "), "\\\\"),
    "\\hline",
    paste(body, "\\\\"),
    "\\hline",
    "\\end{tabular}\\end{table}"), file.path(TAB_DIR, file))
  log_info("wrote tables/%s", file)
}

if (file.exists(F_RESPONSE))
  tex_table(read_result(F_RESPONSE), "table-response-summary.tex",
            paste("Statistical summary of the Zn response before and after the",
                  "logarithm transform and the outlier screen",
                  "(the shape of Table 1 of Song 2022)."),
            "tab:response",
            digits = c(0, 0, 3, 3, 3, 3, 3, 3),
            header = c("Series", "No", "Mean", "Min", "Median", "Max",
                       "$\\sigma$", "CV"))

if (file.exists(F_VARSEL))
  tex_table(read_result(F_VARSEL), "table-variable-selection.tex",
            paste("Correlation with log Zn and variance inflation factors of the",
                  "candidate covariates; the selected set characterises the",
                  "geographical configuration (the shape of Table 2 of Song 2022)."),
            "tab:varsel",
            digits = c(0, 3, 4, 0, 3, 0, 0), sci = "p",
            header = c("Variable", "$R$", "$p$", "Significant", "VIF",
                       "Selected", "In vignette"))

# The comparison tables carry their run metadata (sample count, lambda) in
# columns; those belong in the caption rather than in the body, so only the
# five analysis columns are typeset.
model_table <- function(path, file, label, what) {
  if (!file.exists(path)) return(invisible(NULL))
  cmp <- read_result(path)
  tex_table(cmp[, c("model", "mae", "mae_sd", "rmse", "rmse_sd",
                    "mae_reduction_by_gos_pct", "rmse_reduction_by_gos_pct")],
            file,
            sprintf(paste("Cross-validation errors of the %d models over %d repeated",
                          "50/50 splits of %s (n = %d, $\\lambda$ = %.2f), and the error",
                          "reduction achieved by GOS. The paper's Table 3 also",
                          "benchmarks against ordinary and regression kriging,",
                          "which are outside the scope of this tutorial."),
                    length(CV_MODELS), CV_REPEATS, what, cmp$n_samples[1],
                    cmp$lambda_used_by_gos[1]),
            label,
            digits = c(0, 3, 3, 3, 3, 1, 1),
            header = c("Model", "MAE", "sd(MAE)", "RMSE", "sd(RMSE)",
                       "MAE reduction by GOS (\\%)", "RMSE reduction by GOS (\\%)"))
}

model_table(F_MODELS, "table-model-comparison.tex", "tab:models",
            "the outlier-screened samples")
model_table(F_MODELS_NOSCREEN, "table-model-comparison-no-screening.tex",
            "tab:models-noscreen",
            "all samples, with no outlier screening, as in the paper")

write_session_info()
log_info("wrote env/session-info.txt")
