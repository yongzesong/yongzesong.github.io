# =============================================================================
# 60-tables.R — step 6: manuscript-ready LaTeX tables
# =============================================================================
# Input:  results/*.csv, tables/*.csv
# Output: tables/table-*.tex, ready for \input{} from manuscript/01-main.tex
#
# Numbers reach the manuscript only through these files. Retyping a value into
# the text is how a paper ends up with a Results sentence that contradicts its
# own table.
# =============================================================================

if (!exists("PROJ_ROOT")) source("R/00-config.R")
source(file.path(R_DIR, "01-helpers.R"))

log_step("Step 6/6  Build manuscript tables")

# -- Minimal booktabs writer, no external dependency --------------------------

latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  for (ch in c("&", "%", "$", "#", "_", "{", "}")) {
    x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
  }
  x
}

#' @param escape TRUE for tables holding plain text, FALSE for tables whose
#'   cells and headers are already LaTeX (math symbols, percent signs). Escaping
#'   LaTeX a second time turns $R^2$ into literal dollar signs.
write_latex_table <- function(df, file, caption, label, align = NULL, note = NULL,
                              escape = TRUE) {
  if (is.null(align)) {
    align <- paste0("l", strrep("r", ncol(df) - 1))
  }
  prep <- if (escape) latex_escape else function(x) as.character(x)
  header <- paste(paste0("\\textbf{", prep(names(df)), "}"), collapse = " & ")
  body <- apply(df, 1, function(r) paste(prep(r), collapse = " & "))
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}", label),
    sprintf("\\begin{tabular}{%s}", align),
    "\\hline",
    paste0(header, " \\\\"),
    "\\hline",
    paste0(body, " \\\\"),
    "\\hline",
    "\\end{tabular}",
    if (!is.null(note)) sprintf("\\\\[2pt]\\footnotesize %s", note),
    "\\end{table}"
  )
  writeLines(lines, file)
  log_info("wrote tables/%s", basename(file))
}

# -- Table 1: models ----------------------------------------------------------

mt <- read_result(file.path(TAB_DIR, "table-models.csv"))
write_latex_table(
  data.frame(Type = mt$type, `Model name` = mt$label, `Model code` = mt$model,
             check.names = FALSE),
  file.path(TAB_DIR, "table-models.tex"),
  caption = sprintf("Models evaluated for the spatial prediction of %s.", RESPONSE),
  label = "tab:models", align = "lll"
)

# -- Table 2: accuracy --------------------------------------------------------

acc <- read_result(F_ACCURACY)
write_latex_table(
  data.frame(Model = acc$model,
             `$R^2$` = sprintf("%.3f", acc$R2),
             RMSE = sprintf("%.3f", acc$RMSE),
             MAE = sprintf("%.3f", acc$MAE),
             check.names = FALSE),
  file.path(TAB_DIR, "table-accuracy.tex"),
  caption = "Predictive accuracy of each model on the held-out test set.",
  label = "tab:accuracy", escape = FALSE
)

# -- Table 3: DSI -------------------------------------------------------------
# The counterpart of Table 3 in Liu et al. (2026): the measured spatial
# characteristics of the data and of each residual field, and the resulting
# interpretability values.

dsi <- read_result(F_DSI)
write_latex_table(
  data.frame(
    Model = dsi$model,
    `$\\delta^a_0$` = sprintf("%.3f", dsi$delta0_moran),
    `$\\delta^a_r$` = sprintf("%.3f", dsi$deltar_moran),
    `$\\eta^a$`     = sprintf("%.3f", dsi$eta_autocorrelation),
    `$\\delta^h_0$` = sprintf("%.3f", dsi$delta0_q),
    `$\\delta^h_r$` = sprintf("%.3f", dsi$deltar_q),
    `$\\eta^h$`     = sprintf("%.3f", dsi$eta_heterogeneity),
    check.names = FALSE),
  file.path(TAB_DIR, "table-dsi.tex"),
  caption = paste("Spatial characteristics of the response and of each model's",
                  "residuals, with the resulting degree of spatial",
                  "interpretability for individual characteristics."),
  label = "tab:dsi",
  escape = FALSE,
  note = sprintf("$\\delta^a$ is Moran's I under %d-nearest-neighbour weights; $\\delta^h$ is the Q statistic under %s strata. Subscript 0 denotes the response field and $r$ the residual field.",
                 K_MORAN, STRATIFY_METHOD)
)

# -- Table 4: theta -----------------------------------------------------------

write_latex_table(
  data.frame(
    Model = dsi$model,
    `$\\theta_{min}$`      = sprintf("%.3f", dsi$theta_min),
    `$\\theta_{probable}$` = sprintf("%.3f", dsi$theta_probable),
    `$\\theta_{max}$`      = sprintf("%.3f", dsi$theta_max),
    check.names = FALSE),
  file.path(TAB_DIR, "table-theta.tex"),
  caption = paste("Degree of spatial interpretability for multiple spatial",
                  "characteristics."),
  label = "tab:theta",
  escape = FALSE,
  note = paste("$\\theta_{min}$ is the shared contribution, $\\theta_{probable}$",
               "the dominant characteristic, and $\\theta_{max}$ the value under",
               "independent contributions.")
)

# -- Table 5: accuracy against interpretability -------------------------------

cmp_path <- file.path(RES_DIR, "accuracy-vs-dsi.csv")
if (file.exists(cmp_path)) {
  cmp <- read_result(cmp_path)
  write_latex_table(
    data.frame(Model = cmp$model,
               `$R^2$` = sprintf("%.3f", cmp$R2),
               RMSE = sprintf("%.3f", cmp$RMSE),
               `$\\theta_{probable}$` = sprintf("%.3f", cmp$theta_probable),
               `$\\theta_{max}$` = sprintf("%.3f", cmp$theta_max),
               check.names = FALSE),
    file.path(TAB_DIR, "table-accuracy-vs-dsi.tex"),
    caption = paste("Predictive accuracy against spatial interpretability.",
                    "Models are ordered by $\\theta_{probable}$."),
    label = "tab:accuracy-vs-dsi", escape = FALSE
  )
}

# -- Table 6: DG --------------------------------------------------------------

if (file.exists(F_DG_SUMMARY)) {
  dg <- read_result(F_DG_SUMMARY)
  write_latex_table(
    data.frame(Model = dg$model,
               `Mean DG` = sprintf("%.3f", dg$dg_mean),
               `95\\% CI` = sprintf("[%.3f, %.3f]", dg$dg_ci_low, dg$dg_ci_high),
               `Median DG` = sprintf("%.3f", dg$dg_median),
               `Share $DG>0$` = sprintf("%.1f\\%%", 100 * dg$dg_positive_share),
               check.names = FALSE),
    file.path(TAB_DIR, "table-dg.tex"),
    caption = paste("Point-wise degree of geocomplexity summarised across",
                    "evaluation locations."),
    label = "tab:dg",
    escape = FALSE,
    note = sprintf("Local complexity computed under %d nearest neighbours. The median is reported alongside the mean because the point-wise ratio is heavy-tailed.", K_GEOC)
  )
}

# -- Environment record -------------------------------------------------------

write_session_info()
log_info("wrote env/session-info.txt")
