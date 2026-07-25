# =============================================================================
# fn_experiments.R — the six SOH experiments
# =============================================================================
# Each function maps to one experiment in Ren et al. (2026) and returns a tidy
# data.frame that a figure script consumes without further reshaping.
#
#   soh_exp_individual   Section 4.3, Figure 7        variables vs SOPs alone
#   soh_exp_pairs        Sections 4.4-4.5, Figure 8   SOP-SOP and variable-SOP
#   soh_exp_categories   Sections 4.4-4.5, Figure 8   thematic blocks
#   soh_exp_augmentation Section 4.6, Figure 9        gain from adding SOPs
#   soh_exp_overall      Section 4.6, Figure 10a      scenarios A1 / A2 / A3
#   soh_exp_scale        Section 4.6, Figure 10b      PD against buffer radius
# =============================================================================

#' PD of each covariate alone and of each SOP block alone
soh_exp_individual <- function(points, sopvars, cfg) {
  y <- points$response
  codes <- cfg$variables$codes

  var_rows <- do.call(rbind, lapply(codes, function(v) {
    r <- soh_pd_of(y, points[, v, drop = FALSE], cfg, label = v)
    r$type <- "variable"; r$variable <- v; r
  }))

  sop_rows <- do.call(rbind, lapply(codes, function(v) {
    r <- soh_pd_of(y, sopvars[[v]], cfg, label = paste0("SOP.", v))
    r$type <- "SOP"; r$variable <- v; r
  }))

  out <- rbind(var_rows, sop_rows)
  out$category <- cfg$variables$category[out$variable]
  rownames(out) <- NULL
  out[order(-out$qv), ]
}

#' PD of pairwise interactions
#'
#' @param mode "sop_sop" for SOP block pairs, "var_sop" for covariate x SOP
soh_exp_pairs <- function(points, sopvars, individual, cfg, mode = "sop_sop") {
  y <- points$response
  codes <- cfg$variables$codes

  q_alone <- stats::setNames(individual$qv, individual$label)

  combos <- if (mode == "sop_sop") {
    t(utils::combn(codes, 2))
  } else {
    as.matrix(expand.grid(var = codes, sop = codes, stringsAsFactors = FALSE))
  }

  rows <- lapply(seq_len(nrow(combos)), function(i) {
    a <- combos[i, 1]; b <- combos[i, 2]
    if (mode == "sop_sop") {
      X <- cbind(sopvars[[a]], sopvars[[b]])
      la <- paste0("SOP.", a); lb <- paste0("SOP.", b)
    } else {
      X <- cbind(points[, a, drop = FALSE], sopvars[[b]])
      la <- a; lb <- paste0("SOP.", b)
    }
    r <- soh_pd_of(y, X, cfg, label = paste(la, lb, sep = " & "))
    r$factor1 <- la; r$factor2 <- lb
    r$q1 <- unname(q_alone[la]); r$q2 <- unname(q_alone[lb])
    r$interaction <- soh_interaction_type(r$q1, r$q2, r$qv)
    r
  })

  out <- do.call(rbind, rows)
  out$mode <- mode
  rownames(out) <- NULL
  out[order(-out$qv), ]
}

#' PD of thematic category blocks and their interactions
soh_exp_categories <- function(points, sopvars, cfg) {
  y <- points$response
  cats <- unique(cfg$variables$category)
  vars_of <- function(cat) names(cfg$variables$category)[cfg$variables$category == cat]

  sop_block <- function(cat) do.call(cbind, sopvars[vars_of(cat)])
  var_block <- function(cat) points[, vars_of(cat), drop = FALSE]

  single <- do.call(rbind, lapply(cats, function(c1) {
    rbind(
      cbind(soh_pd_of(y, var_block(c1), cfg, label = c1), type = "variable category"),
      cbind(soh_pd_of(y, sop_block(c1), cfg, label = paste0("SOP.", c1)), type = "SOP category")
    )
  }))

  q_alone <- stats::setNames(single$qv, single$label)

  pairs_sop <- t(utils::combn(cats, 2))
  sop_sop <- do.call(rbind, lapply(seq_len(nrow(pairs_sop)), function(i) {
    a <- pairs_sop[i, 1]; b <- pairs_sop[i, 2]
    r <- soh_pd_of(y, cbind(sop_block(a), sop_block(b)), cfg,
                   label = paste0("SOP.", a, " & SOP.", b))
    r$factor1 <- paste0("SOP.", a); r$factor2 <- paste0("SOP.", b)
    r$type <- "SOP category x SOP category"
    r$q1 <- unname(q_alone[r$factor1]); r$q2 <- unname(q_alone[r$factor2])
    r$interaction <- soh_interaction_type(r$q1, r$q2, r$qv)
    r
  }))

  grid_vs <- expand.grid(v = cats, s = cats, stringsAsFactors = FALSE)
  var_sop <- do.call(rbind, lapply(seq_len(nrow(grid_vs)), function(i) {
    a <- grid_vs$v[i]; b <- grid_vs$s[i]
    r <- soh_pd_of(y, cbind(var_block(a), sop_block(b)), cfg,
                   label = paste0(a, " & SOP.", b))
    r$factor1 <- a; r$factor2 <- paste0("SOP.", b)
    r$type <- "variable category x SOP category"
    r$q1 <- unname(q_alone[r$factor1]); r$q2 <- unname(q_alone[r$factor2])
    r$interaction <- soh_interaction_type(r$q1, r$q2, r$qv)
    r
  }))

  single$factor1 <- single$label; single$factor2 <- NA_character_
  single$q1 <- NA_real_; single$q2 <- NA_real_; single$interaction <- NA_character_
  out <- rbind(single, sop_sop, var_sop)
  rownames(out) <- NULL
  out
}

#' Gain in PD from adding the SOP of a variable to that variable
#'
#' Reported at variable level and at category level, which is Figure 9 of the
#' reference study. A positive delta is the core claim of the SOH model.
soh_exp_augmentation <- function(points, sopvars, cfg) {
  y <- points$response
  codes <- cfg$variables$codes

  by_var <- do.call(rbind, lapply(codes, function(v) {
    a <- soh_pd_of(y, points[, v, drop = FALSE], cfg, label = v)
    b <- soh_pd_of(y, cbind(points[, v, drop = FALSE], sopvars[[v]]), cfg,
                   label = paste0(v, "+SOP"))
    data.frame(level = "variable", unit = v,
               qv_base = a$qv, qv_augmented = b$qv,
               delta = b$qv - a$qv,
               delta_pct = 100 * (b$qv - a$qv) / a$qv,
               sig_base = a$sig, sig_augmented = b$sig,
               stringsAsFactors = FALSE)
  }))

  cats <- unique(cfg$variables$category)
  by_cat <- do.call(rbind, lapply(cats, function(c1) {
    vs <- names(cfg$variables$category)[cfg$variables$category == c1]
    a <- soh_pd_of(y, points[, vs, drop = FALSE], cfg, label = c1)
    b <- soh_pd_of(y, cbind(points[, vs, drop = FALSE], do.call(cbind, sopvars[vs])),
                   cfg, label = paste0(c1, "+SOP"))
    data.frame(level = "category", unit = c1,
               qv_base = a$qv, qv_augmented = b$qv,
               delta = b$qv - a$qv,
               delta_pct = 100 * (b$qv - a$qv) / a$qv,
               sig_base = a$sig, sig_augmented = b$sig,
               stringsAsFactors = FALSE)
  }))

  rbind(by_var, by_cat)
}

#' Overall PD under the three predictor scenarios
#'
#'   A1  SOPs only            the second dimension alone
#'   A2  variables + SOPs     both dimensions
#'   A3  variables only       the conventional first-dimension model
soh_exp_overall <- function(points, sopvars, cfg) {
  y <- points$response
  codes <- cfg$variables$codes
  Xv <- points[, codes, drop = FALSE]
  Xs <- soh_sop_wide(sopvars)

  rbind(
    cbind(soh_pd_of(y, Xs, cfg, label = "A1 SOP only"), scenario = "A1"),
    cbind(soh_pd_of(y, cbind(Xv, Xs), cfg, label = "A2 variables + SOP"), scenario = "A2"),
    cbind(soh_pd_of(y, Xv, cfg, label = "A3 variables only"), scenario = "A3")
  )
}

#' PD as a function of the largest neighbourhood radius
#'
#' SOPs are generated once at the largest radius in the sweep and then
#' truncated, so this costs one SOP pass rather than one per radius. The
#' variable-only scenario is scale invariant by construction and is repeated
#' across radii as a flat reference line.
soh_exp_scale <- function(points, sopvars_full, cfg, sweep = NULL,
                          buffers_full = NULL) {
  sweep <- sweep %||% cfg$scale_sweep
  buffers_full <- buffers_full %||% cfg$sop$buffers
  y <- points$response
  codes <- cfg$variables$codes
  Xv <- points[, codes, drop = FALSE]
  q_var <- soh_pd_of(y, Xv, cfg)$qv

  do.call(rbind, lapply(sweep, function(r) {
    sub <- lapply(sopvars_full, soh_sop_subset_buffers,
                  buffers = buffers_full, rmax = r)
    if (!ncol(sub[[1]])) return(NULL)
    Xs <- soh_sop_wide(sub)
    q_sop <- soh_pd_of(y, Xs, cfg)$qv
    q_both <- soh_pd_of(y, cbind(Xv, Xs), cfg)$qv
    data.frame(
      radius = r,
      scenario = c("A1 SOP only", "A2 variables + SOP", "A3 variables only"),
      qv = c(q_sop, q_both, q_var),
      stringsAsFactors = FALSE
    )
  }))
}

#' PD under alternative outlier thresholds
#'
#' Regenerates SOPs at each sd threshold, so this is the slowest experiment.
#' It is the reproducibility check that the published conclusions do not hinge
#' on the choice of 2 standard deviations.
soh_exp_threshold <- function(points, grids, cfg, sweep = NULL) {
  sweep <- sweep %||% cfg$threshold_sweep
  y <- points$response
  codes <- cfg$variables$codes
  Xv <- points[, codes, drop = FALSE]

  do.call(rbind, lapply(sweep, function(s) {
    soh_log("  threshold sweep: sd = ", s)
    sv <- soh_generate_sop_all(points, grids, cfg, sd_threshold = s,
                               progress = FALSE)
    Xs <- soh_sop_wide(sv)
    data.frame(
      sd_threshold = s,
      scenario = c("A1 SOP only", "A2 variables + SOP", "A3 variables only"),
      qv = c(soh_pd_of(y, Xs, cfg)$qv,
             soh_pd_of(y, cbind(Xv, Xs), cfg)$qv,
             soh_pd_of(y, Xv, cfg)$qv),
      stringsAsFactors = FALSE
    )
  }))
}
