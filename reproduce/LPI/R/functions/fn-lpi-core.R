# =============================================================================
# fn-lpi-core.R — the generalizable LPI method core
# Source: cc005 LPI pipeline (main_analysis.R), generalized and config-driven.
#
# Components (in pipeline order):
#   build_spatial()          projected coordinates + full distance matrix
#   estimate_local_range()   semivariogram range -> local extent (threshold)
#   run_lisp_factor()        LISP local q per variable (localsp::lisp)
#   run_local_interaction()  local pairwise interaction q + 5-class typing
#   run_interactions_all()   parallel loop of run_local_interaction()
#   run_gozh()               GOZH total effect: rpart zones -> gdverse::gd
#   run_local_gozh()         local GOZH over k-nearest neighbourhoods
#   run_global_gd()          OPGD-style global q for one variable
#   run_global_interaction_q()  global q for one variable pair
# =============================================================================

# --- Spatial scaffolding -----------------------------------------------------

# Projected coordinate matrix (metres) and full pairwise distance matrix.
build_spatial <- function(d, cfg) {
  sf_pts  <- sf::st_as_sf(d, coords = c(cfg$data$x_col, cfg$data$y_col),
                          crs = cfg$project$crs_input)
  sf_proj <- sf::st_transform(sf_pts, crs = cfg$project$crs_projected)
  coords_mat <- sf::st_coordinates(sf_proj)
  list(sf_proj    = sf_proj,
       coords_mat = coords_mat,
       distmat    = as.matrix(dist(coords_mat)))
}

# Semivariogram of the response on projected coordinates; the local extent
# (distance threshold) is multiplier * range, following LISP (Hu et al. 2024).
estimate_local_range <- function(d, cfg, spatial = NULL) {
  if (is.null(spatial)) spatial <- build_spatial(d, cfg)
  response <- cfg$response$response_name
  sp_obj <- as(spatial$sf_proj, "Spatial")
  v <- automap::autofitVariogram(
    as.formula(paste(response, "~ 1")), sp_obj
  )
  vg_range  <- v$var_model$range[2]
  threshold <- vg_range * cfg$params$lisp$threshold_multiplier
  list(variogram = v, range = vg_range, threshold = threshold)
}

# --- LISP factor detector ------------------------------------------------

# Local q (power of determinant) of every variable at every location.
# Returns list(q = data.frame, sig = data.frame) with NA imputed:
# q -> 0 (no explanatory power), p -> 1 (not significant).
run_lisp_factor <- function(d, cfg, vars, threshold, distmat,
                            discnum = NULL, cores = NULL) {
  response <- cfg$response$response_name
  if (is.null(discnum)) discnum <- cfg$params$lisp$discnum
  if (is.null(cores))   cores   <- get_cores(cfg)

  d_lisp <- d[, c(response, vars)]
  lisp_result <- localsp::lisp(
    formula    = as.formula(paste(response, "~ .")),
    data       = d_lisp,
    threshold  = threshold,
    distmat    = distmat,
    discnum    = discnum,
    discmethod = cfg$params$lisp$discmethod,
    cores      = cores
  )
  lisp_df <- as.data.frame(lisp_result)

  pd_cols  <- grep("^pd_",  names(lisp_df), value = TRUE)
  sig_cols <- grep("^sig_", names(lisp_df), value = TRUE)

  base_cols <- data.frame(
    LocationID = seq_len(nrow(d)),
    Longitude  = d[[cfg$data$x_col]],
    Latitude   = d[[cfg$data$y_col]]
  )
  local_q   <- cbind(base_cols, lisp_df[, pd_cols,  drop = FALSE])
  local_sig <- cbind(base_cols, lisp_df[, sig_cols, drop = FALSE])
  names(local_q)[-(1:3)]   <- sub("^pd_",  "", pd_cols)
  names(local_sig)[-(1:3)] <- sub("^sig_", "", sig_cols)

  var_cols <- setdiff(names(local_q), c("LocationID", "Longitude", "Latitude"))
  local_q[var_cols]   <- lapply(local_q[var_cols],
                                function(x) { x[is.na(x)] <- 0;   x })
  local_sig[var_cols] <- lapply(local_sig[var_cols],
                                function(x) { x[is.na(x)] <- 1.0; x })

  list(q = local_q, sig = local_sig)
}

# --- Local pairwise interaction detector ----------------------------------

classify_interaction <- function(qv1, qv2, qv12) {
  dplyr::case_when(
    qv12 < min(qv1, qv2)                          ~ "Weaken, nonlinear",
    qv12 >= min(qv1, qv2) & qv12 < max(qv1, qv2)  ~ "Weaken, uni-",
    qv12 == qv1 + qv2                             ~ "Independent",
    qv12 > max(qv1, qv2) & qv12 < qv1 + qv2       ~ "Enhance, bi-",
    qv12 > qv1 + qv2                              ~ "Enhance, nonlinear",
    TRUE                                          ~ NA_character_
  )
}

# One location: local window by distance threshold (fallback: min_local_n
# nearest points), quantile discretization of each pair member, single and
# combined-zone q via gdverse::gd, then 5-class interaction typing.
run_local_interaction <- function(i, data, distmat, threshold,
                                  response, var_pairs, discnum = 4,
                                  min_local_n = 10) {
  dists     <- distmat[i, ]
  local_idx <- which(dists <= threshold)
  if (length(local_idx) < min_local_n) {
    local_idx <- order(dists)[1:min(min_local_n, nrow(data))]
  }
  local_data <- data[local_idx, , drop = FALSE]

  empty_row <- function(v1, v2) {
    data.frame(var1 = v1, var2 = v2,
               qv1 = NA, qv2 = NA, qv12 = NA, pv12 = NA,
               interaction = NA, LocationID = i, stringsAsFactors = FALSE)
  }

  pair_results <- lapply(var_pairs, function(pair) {
    v1 <- pair[1]; v2 <- pair[2]
    if (length(unique(local_data[[v1]])) < 2 ||
        length(unique(local_data[[v2]])) < 2) {
      return(empty_row(v1, v2))
    }

    tryCatch({
      disc_v1 <- tryCatch(
        sdsfun::discretize_vector(local_data[[v1]], n = discnum,
                                  method = "quantile"),
        error = function(e) NULL)
      disc_v2 <- tryCatch(
        sdsfun::discretize_vector(local_data[[v2]], n = discnum,
                                  method = "quantile"),
        error = function(e) NULL)
      if (is.null(disc_v1) || is.null(disc_v2)) stop("discretization failed")

      tmp <- local_data
      tmp[["disc_v1"]]  <- disc_v1
      tmp[["disc_v2"]]  <- disc_v2
      tmp[["disc_v12"]] <- paste(disc_v1, disc_v2, sep = "_")

      y <- tmp[[response]]
      if (sum((y - mean(y))^2) == 0) stop("zero total SS")

      gd_v1  <- gdverse::gd(as.formula(paste(response, "~ disc_v1")),  data = tmp)
      gd_v2  <- gdverse::gd(as.formula(paste(response, "~ disc_v2")),  data = tmp)
      gd_v12 <- gdverse::gd(as.formula(paste(response, "~ disc_v12")), data = tmp)

      qv1  <- as.numeric(gd_v1$factor[["Q-statistic"]])
      qv2  <- as.numeric(gd_v2$factor[["Q-statistic"]])
      qv12 <- as.numeric(gd_v12$factor[["Q-statistic"]])
      pv12 <- as.numeric(gd_v12$factor[["P-value"]])

      data.frame(var1 = v1, var2 = v2,
                 qv1 = qv1, qv2 = qv2, qv12 = qv12, pv12 = pv12,
                 interaction = classify_interaction(qv1, qv2, qv12),
                 LocationID = i, stringsAsFactors = FALSE)
    }, error = function(e) empty_row(v1, v2))
  })

  do.call(rbind, pair_results)
}

# Parallel driver over all locations. Returns one long data.frame.
run_interactions_all <- function(d, cfg, var_pairs, distmat, threshold) {
  response    <- cfg$response$response_name
  discnum     <- cfg$params$lisp$discnum
  min_local_n <- cfg$params$interaction$min_local_n
  n_cores     <- get_cores(cfg)

  worker <- function(i) {
    run_local_interaction(
      i = i, data = d, distmat = distmat, threshold = threshold,
      response = response, var_pairs = var_pairs,
      discnum = discnum, min_local_n = min_local_n
    )
  }

  if (n_cores > 1) {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(
      cl,
      varlist = c("d", "distmat", "threshold", "response", "var_pairs",
                  "discnum", "min_local_n",
                  "run_local_interaction", "classify_interaction"),
      envir = environment()
    )
    parallel::clusterEvalQ(cl, {
      library(gdverse); library(sdsfun); library(dplyr)
    })
    inter_list <- parallel::parLapply(cl, seq_len(nrow(d)), worker)
  } else {
    inter_list <- lapply(seq_len(nrow(d)), function(i) {
      if (i %% 200 == 0) cat(sprintf("  interaction: location %d / %d\n",
                                     i, nrow(d)))
      worker(i)
    })
  }

  do.call(rbind, Filter(Negate(is.null), inter_list))
}

# --- GOZH total effect -----------------------------------------------------

# rpart regression tree on all predictors -> leaf zones -> combined-zone q.
run_gozh <- function(data, response, predictors) {
  formula_tree <- as.formula(
    paste(response, "~", paste(predictors, collapse = " + "))
  )
  tree <- tryCatch(rpart::rpart(formula_tree, data = data, method = "anova"),
                   error = function(e) NULL)
  if (is.null(tree)) return(c(q = NA_real_, p = NA_real_))

  data$.zone <- as.character(as.numeric(tree$where))
  result <- tryCatch(
    gdverse::gd(as.formula(paste(response, "~ .zone")), data = data),
    error = function(e) NULL)
  if (is.null(result)) return(c(q = NA_real_, p = NA_real_))

  qv <- tryCatch(as.numeric(result$factor[["Q-statistic"]]),
                 error = function(e) NA_real_)
  pv <- tryCatch(as.numeric(result$factor[["P-value"]]),
                 error = function(e) NA_real_)
  if (length(qv) == 0) qv <- NA_real_
  if (length(pv) == 0) pv <- NA_real_
  c(q = qv, p = pv)
}

# Local GOZH: run_gozh() inside each location's k-nearest neighbourhood.
run_local_gozh <- function(d, cfg, predictors, coords_mat, k_local) {
  response <- cfg$response$response_name
  n <- nrow(d)
  local_q <- rep(NA_real_, n)
  local_p <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (i %% 200 == 0) cat(sprintf("  local GOZH: location %d / %d\n", i, n))
    nn_idx <- FNN::get.knnx(coords_mat, coords_mat[i, , drop = FALSE],
                            k = k_local)$nn.index
    res <- run_gozh(d[nn_idx, ], response, predictors)
    local_q[i] <- res["q"]
    local_p[i] <- res["p"]
  }
  data.frame(local_gozh = local_q, local_gozh_p = local_p)
}

# --- Global benchmarks (OPGD-style) ----------------------------------------

run_global_gd <- function(data, response, predictor, n_strata = 5) {
  tryCatch({
    df <- data.frame(y = data[[response]], x = data[[predictor]])
    df <- df[complete.cases(df), ]
    if (nrow(df) < 30) return(c(q = NA_real_, p = NA_real_))
    df$x_d <- cut(df$x, breaks = n_strata, labels = FALSE,
                  include.lowest = TRUE)
    result <- gdverse::gd(y ~ x_d, data = df)
    qv <- tryCatch(as.numeric(result$factor[["Q-statistic"]]),
                   error = function(e) NA_real_)
    pv <- tryCatch(as.numeric(result$factor[["P-value"]]),
                   error = function(e) NA_real_)
    if (length(qv) == 0) qv <- NA_real_
    if (length(pv) == 0) pv <- NA_real_
    c(q = qv, p = pv)
  }, error = function(e) c(q = NA_real_, p = NA_real_))
}

run_global_interaction_q <- function(data, response, v1, v2, n_strata = 5) {
  tryCatch({
    df <- data.frame(y = data[[response]], x1 = data[[v1]], x2 = data[[v2]])
    df <- df[complete.cases(df), ]
    if (nrow(df) < 30) return(c(q = NA_real_, p = NA_real_))
    df$x1_d <- cut(df$x1, breaks = n_strata, labels = FALSE,
                   include.lowest = TRUE)
    df$x2_d <- cut(df$x2, breaks = n_strata, labels = FALSE,
                   include.lowest = TRUE)
    df$x12 <- interaction(df$x1_d, df$x2_d, drop = TRUE)
    result <- gdverse::gd(y ~ x12, data = df)
    qv <- tryCatch(as.numeric(result$factor[["Q-statistic"]]),
                   error = function(e) NA_real_)
    pv <- tryCatch(as.numeric(result$factor[["P-value"]]),
                   error = function(e) NA_real_)
    if (length(qv) == 0) qv <- NA_real_
    if (length(pv) == 0) pv <- NA_real_
    c(q = qv, p = pv)
  }, error = function(e) c(q = NA_real_, p = NA_real_))
}
