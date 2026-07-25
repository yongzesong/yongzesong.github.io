# =============================================================================
# 03-dsi.R — degree of spatial interpretability
# =============================================================================
# Liu, H., Song, Y., & Yi, W. (2026). Degree of spatial interpretability.
# International Journal of Geographical Information Science.
# https://doi.org/10.1080/13658816.2026.2614335
#
#   eta   = (delta_0 - delta_r) / delta_0            one characteristic  (Eq. 1)
#   theta_min      = min(eta_1, ..., eta_n)          shared contribution (Eq. 2)
#   theta_probable = max(eta_1, ..., eta_n)          dominant characteristic
#   theta_max      = 1 - prod(1 - eta_i)             independent contributions
#
# delta_0 is a spatial characteristic measured on the response, delta_r the
# same characteristic measured on the model residuals. The indicator is an
# attenuation ratio: how much of the structure present in the data has been
# absorbed by the model rather than passed through to its residuals.
# =============================================================================

#' Degree of spatial interpretability for one spatial characteristic.
#'
#' @param delta_0 characteristic measured on the response field
#' @param delta_r the same characteristic measured on the residual field
#' @return attenuation in [.., 1]; 1 means the residual field retains none of
#'   the structure, 0 means the model absorbed none of it, negative means the
#'   model amplified it
dsi_eta <- function(delta_0, delta_r) {
  if (is.na(delta_0) || is.na(delta_r)) return(NA_real_)
  if (abs(delta_0) < 1e-8) {
    log_warn("delta_0 is ~0; the response carries no measurable structure to explain")
    return(NA_real_)
  }
  (delta_0 - delta_r) / delta_0
}

#' Degree of spatial interpretability across several characteristics.
#'
#' The three values are a diagnostic range, not three competing estimates.
#' theta_min is what the model explains if the characteristics overlap
#' completely, theta_max if they are independent, and theta_probable is the
#' dominant characteristic. A wide gap between theta_min and theta_max is
#' itself the finding: the model is strong on one characteristic and weak on
#' another.
dsi_theta <- function(etas) {
  etas <- etas[!is.na(etas)]
  if (!length(etas)) {
    return(list(theta_min = NA_real_, theta_probable = NA_real_, theta_max = NA_real_))
  }
  list(
    theta_min      = min(etas),
    theta_probable = max(etas),
    theta_max      = 1 - prod(1 - etas)
  )
}

#' Full DSI row for one model.
#'
#' @param response numeric vector of observed values on the evaluation set
#' @param residual numeric vector of that model's residuals, same ordering
#' @param listw spatial weights from build_knn_weights()
#' @param strata strata from make_strata()
dsi_for_model <- function(model_name, response, residual, listw, strata) {
  obs <- measure_field(response, listw, strata)
  res <- measure_field(residual, listw, strata)

  eta_a <- dsi_eta(obs$moran, res$moran)
  eta_h <- dsi_eta(obs$q,     res$q)
  theta <- dsi_theta(c(eta_a, eta_h))

  data.frame(
    model          = model_name,
    delta0_moran   = obs$moran,
    deltar_moran   = res$moran,
    moran_p_data   = obs$moran_p,
    moran_p_resid  = res$moran_p,
    delta0_q       = obs$q,
    deltar_q       = res$q,
    eta_autocorrelation = eta_a,
    eta_heterogeneity   = eta_h,
    theta_min      = theta$theta_min,
    theta_probable = theta$theta_probable,
    theta_max      = theta$theta_max
  )
}

#' DSI table across all models.
#'
#' @param residuals data frame with one column per model, rows aligned with
#'   `response` and with the coordinates used to build `listw`
dsi_table <- function(response, residuals, listw, strata) {
  rows <- lapply(names(residuals), function(m) {
    dsi_for_model(m, response, residuals[[m]], listw, strata)
  })
  out <- do.call(rbind, rows)
  out[order(-out$theta_max, -out$theta_probable), ]
}

#' Flag results whose interpretation needs a caveat in the manuscript.
#'
#' Three situations recur when the template is ported to a new domain, and each
#' one changes what the paper is allowed to claim.
dsi_diagnostics <- function(dsi) {
  notes <- character(0)
  if (any(dsi$moran_p_data > 0.05, na.rm = TRUE)) {
    notes <- c(notes, paste0(
      "The response shows no significant spatial autocorrelation (p > 0.05). ",
      "eta for autocorrelation is not interpretable; report the heterogeneity ",
      "component alone or choose a characteristic the data actually carries."))
  }
  neg <- dsi$model[dsi$eta_autocorrelation < 0 | dsi$eta_heterogeneity < 0]
  neg <- neg[!is.na(neg)]
  if (length(neg)) {
    notes <- c(notes, paste0(
      "Negative eta for: ", paste(neg, collapse = ", "),
      ". These models leave residuals with more structure than the response ",
      "itself, which is a substantive finding and should be discussed, not hidden."))
  }
  wide <- dsi$model[(dsi$theta_max - dsi$theta_min) > 0.5]
  wide <- wide[!is.na(wide)]
  if (length(wide)) {
    notes <- c(notes, paste0(
      "Wide theta range for: ", paste(wide, collapse = ", "),
      ". Each of these models handles one spatial characteristic well and the ",
      "other poorly; name which is which in the Results section."))
  }
  if (!length(notes)) notes <- "No diagnostic flags raised."
  notes
}
