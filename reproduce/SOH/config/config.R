# =============================================================================
# config.R — the single place where a new study is configured
# =============================================================================
# Everything that changes when the SOH model is ported to a new domain lives in
# this file. No other script should contain a study-specific path, variable
# name, buffer distance, or threshold. Edit this file first, then run
# code/99_run_all.R.
#
# Reference implementation: Ren et al. (2026) IJGIS, Australian barley.
# The commented "# barley:" values record what the reference study used, so a
# new study can see the order of magnitude expected for each setting.
# =============================================================================

cfg <- list(

  # ---------------------------------------------------------------------------
  # 1. Study identity
  # ---------------------------------------------------------------------------
  study = list(
    id          = "demo",                    # short slug, used in file names
    title       = "SOH demo study",          # human readable
    domain      = "synthetic demonstration", # barley: agricultural production
    response_label = "Response (unit)",      # barley: "Barley production (ton/km2)"
    author      = "YOUR NAME",
    date        = format(Sys.Date(), "%Y-%m-%d")
  ),

  # ---------------------------------------------------------------------------
  # 2. Data mode
  # ---------------------------------------------------------------------------
  # "demo" — generate a synthetic dataset with known outlier structure so the
  #          whole pipeline runs end to end without any external download.
  # "user" — read data/processed/points.csv and data/processed/grids.csv
  #          which you prepare in 01_prepare_data.R.
  data = list(
    mode = "demo",

    # File names inside data/processed/ used in "user" mode.
    points_file = "points.csv",   # one row per analysis unit (the SOH sample)
    grids_file  = "grids.csv",    # one row per fine grid cell (the second dimension)

    # Column names. The template renames these internally to id / x / y / value.
    id_col   = "id",
    x_col    = "x",               # projected easting, NOT longitude
    y_col    = "y",               # projected northing, NOT latitude
    response_col = "response",    # present in points only

    # Coordinate reference system of x/y. Must be projected with metric units,
    # because buffer radii are expressed as distances.
    crs_epsg = 3577,              # barley: 3577 (GDA94 / Australian Albers)
    dist_unit = "km"              # unit of x/y AND of buffer radii below
  ),

  # ---------------------------------------------------------------------------
  # 3. Explanatory variables and their categories
  # ---------------------------------------------------------------------------
  # Each variable must exist as a column in BOTH points and grids.
  # Categories group variables into thematic blocks (the reference study used
  # climate / topography / soil / vegetation) and drive the category-level
  # interaction analysis.
  variables = list(
    codes = c("V1", "V2", "V3", "V4", "V5", "V6"),
    labels = c(
      V1 = "Variable 1", V2 = "Variable 2", V3 = "Variable 3",
      V4 = "Variable 4", V5 = "Variable 5", V6 = "Variable 6"
    ),
    category = c(
      V1 = "C1", V2 = "C1",       # barley C1: climate  (AT, TP, VPD)
      V3 = "C2", V4 = "C2",       # barley C2: topography (ELE)
      V5 = "C3", V6 = "C3"        # barley C3: soil, C4: vegetation
    ),
    category_labels = c(
      C1 = "Category 1", C2 = "Category 2", C3 = "Category 3"
    )
  ),

  # ---------------------------------------------------------------------------
  # 4. SOP construction (second-dimension outlier patterns)
  # ---------------------------------------------------------------------------
  sop = list(
    # Neighbourhood radii, in cfg$data$dist_unit. Ren et al. (2026) used
    # seq(20, 200, 20) km for barley. Ren et al. (2025) recommend 5-10 buffers
    # spanning 10-20% of the maximum pairwise distance between analysis units;
    # code/02_generate_sops.R prints that diagnostic before it runs.
    buffers = seq(20, 200, 20),

    # Outlier threshold in standard deviations within each neighbourhood.
    # 2 reproduces the published model; 1.5 and 2.5 are the sensitivity values.
    sd_threshold = 2,

    # How an outlier set is summarised into one number.
    #   "zsum"   — sum of z scores beyond the threshold (published behaviour)
    #   "rawsum" — sum of raw deviations from the neighbourhood mean
    #   "count"  — number of outlier cells
    statistic = "zsum",

    # Minimum cells inside a neighbourhood before an SOP is computed. Below
    # this the SOP is set to 0, because a z score over 3 cells is meaningless.
    min_cells = 10,

    # Cap on grid cells per neighbourhood; 0 disables. Large radii over dense
    # grids are the main cost of the whole pipeline.
    max_cells = 0
  ),

  # ---------------------------------------------------------------------------
  # 5. Stratification and power of determinant
  # ---------------------------------------------------------------------------
  pd = list(
    cart_cp        = 0.01,   # rpart complexity parameter (published value)
    cart_minsplit  = 20,     # rpart default
    cart_minbucket = 7,      # rpart default; raise for small samples
    min_stratum_n  = 2,      # strata smaller than this are pooled before q
    n_top_pairs    = 30      # rows kept in the ranked interaction tables
  ),

  # ---------------------------------------------------------------------------
  # 6. Experiments to run
  # ---------------------------------------------------------------------------
  experiments = list(
    moran            = TRUE,  # global Moran's I of the response
    individual       = TRUE,  # PD of each variable and each SOP block
    pairs_sop_sop    = TRUE,  # SOP x SOP interactions
    pairs_var_sop    = TRUE,  # variable x SOP interactions
    categories       = TRUE,  # category-level blocks and interactions
    augmentation     = TRUE,  # variable vs variable+SOP gain
    overall          = TRUE,  # scenarios A1 SOP-only / A2 combined / A3 variable-only
    scale_sensitivity = TRUE, # PD as a function of buffer radius
    threshold_sensitivity = TRUE  # PD under sd_threshold 1.5 / 2 / 2.5
  ),

  # Buffer radii used for the scale-sensitivity sweep. Each entry re-generates
  # SOPs using buffers up to that radius, so this is the slowest experiment.
  scale_sweep = seq(20, 200, 20),

  # sd thresholds for the threshold-sensitivity experiment.
  threshold_sweep = c(1.5, 2, 2.5),

  # ---------------------------------------------------------------------------
  # 7. Reproducibility and output
  # ---------------------------------------------------------------------------
  run = list(
    seed = 20260724,
    verbose = TRUE,
    save_intermediate = TRUE,
    figure_device = "png",   # "png" or "pdf"
    figure_dpi = 300,
    figure_width = 180,      # mm, single column 90 / double column 180
    figure_height = 120
  ),

  # ---------------------------------------------------------------------------
  # 8. Synthetic demo settings (ignored when data$mode is "user")
  # ---------------------------------------------------------------------------
  demo = list(
    grid_n        = 60,    # grid is grid_n x grid_n cells
    grid_extent   = 600,   # full grid side length, in dist_unit
    # Analysis units are sampled only from the interior, leaving this margin of
    # grid on every side. Keep it at or above half the largest buffer radius so
    # no unit has a truncated neighbourhood.
    point_margin  = 100,
    n_points      = 300,   # number of analysis units
    n_anomaly     = 24,    # number of local anomaly centres injected
    anomaly_radius = 25,   # radius of each anomaly, in dist_unit
    anomaly_strength = 3.5,# anomaly magnitude in SD of the background field
    noise_sd      = 0.35,  # observation noise on the response
    # Weight of the outlier signal relative to the smooth covariate signal in
    # the response. 0 means SOPs carry no information (a useful null check);
    # 0.6 means the second dimension dominates.
    outlier_signal_weight = 0.55
  )
)

cfg
