# Notes for the tutorial page author

Everything below was produced by `Rscript run-all.R` in this directory on
R 4.6.0 with `geosimilarity` 3.9, seed 42, one core. Total runtime **107.0 s**.
Both test scripts pass: **18/18** in `tests/test-01-method-properties.R` and
**24/24** in `tests/test-02-reproduce-pipeline.R` (`Rscript run-all.R test`,
about 40 s).

Method reference throughout: Song, Y. (2022), *Mathematical Geosciences*
55:295-320, doi:10.1007/s11004-022-10036-8. Equation numbers are the paper's.

---

## 1. What each output file holds

### `results/`

| file | one-line description |
|---|---|
| `response-summary.csv` | n, mean, min, median, max, sd and CV of Zn in ppm and of log Zn, before and after the outlier screen — the shape of the paper's Table 1. |
| `variable-selection.csv` | All nine candidate covariates with their correlation with log Zn (r, p), whether they passed the p < 0.05 screen, their final VIF, whether they were selected, and whether the package vignette also uses them — the shape of the paper's Table 2. |
| `kappa-rmse.csv` | The 19 candidate kappa values with mean and sd of the cross-validation RMSE over 10 repeats, and a `selected` flag on lambda — the data behind the paper's Fig. 7. |
| `grid-prediction.csv` | GOS prediction at kappa = lambda for all 13,132 grid cells: `GridID`, `Lon`, `Lat`, `pred` (log Zn), `pred_zn_ppm` (= exp(pred)), and all six Eq. 12 uncertainty columns. |
| `grid-prediction-bcs.csv` | The same columns for the BCS prediction (kappa = 1), for the contrast in Fig. 5. |
| `model-comparison.csv` | Mean and sd of MAE and RMSE for MLR, BCS and GOS over 50 repeated 50/50 splits of the **outlier-screened** 885 samples, plus GOS's percentage error reduction against each. Also carries `dataset`, `n_samples` and `lambda_used_by_gos`. |
| `model-comparison-no-outlier-screening.csv` | The identical comparison on all **894** samples with no outlier screening, which is the paper's own preprocessing, with lambda re-derived for that sample set. |
| `model-comparison-repeats.csv` | The per-repeat MAE and RMSE behind both tables above (300 rows: 2 datasets x 50 repeats x 3 models), with a `dataset` column. This is what the boxplots in Fig. 6 are drawn from. |

### `tables/`

LaTeX versions of the same numbers: `table-response-summary.tex`,
`table-variable-selection.tex`, `table-model-comparison.tex` and
`table-model-comparison-no-screening.tex`.

### `data/`, `env/`

`data/zn-samples.csv` and `data/grid-covariates.csv` are verbatim copies of the
packaged inputs; `data/provenance.md` says where they come from and what is
missing from them. `env/requirements.md` has the software list, install line,
per-step runtimes and the scope caveat; `env/session-info.txt` and
`env/runtimes.csv` are written by the run itself.

---

## 2. Headline numbers

### The response (step 10, `results/response-summary.csv`)

| series | n | mean | min | median | max | sd | CV |
|---|---|---|---|---|---|---|---|
| Zn, ppm, all samples | 894 | 42.55 | 2 | 29 | 353.5 | 37.11 | 0.872 |
| log Zn, all samples | 894 | 3.436 | 0.693 | 3.367 | 5.868 | 0.801 | 0.233 |
| Zn, ppm, screened | 885 | 41.64 | 5 | 29 | 181 | 32.95 | 0.791 |
| log Zn, screened | 885 | 3.438 | 1.609 | 3.367 | 5.198 | 0.772 | 0.225 |

`removeoutlier(coef = 2.5)` drops **9 of 894** rows, leaving **885**. The log
transform works: Shapiro-Wilk W = 0.986 on the screened log Zn, skewness 0.04.

### Selected variables (step 20, `results/variable-selection.csv`)

Seven of the nine candidates are significantly correlated with log Zn and all
seven survive the VIF < 4 screen unchanged — no variable is dropped for
collinearity.

| variable | r | p | VIF |
|---|---|---|---|
| NDVI | -0.398 | 5.4e-35 | 1.460 |
| SOC | -0.317 | 4.7e-22 | 1.356 |
| Mine | -0.259 | 5.4e-15 | 2.608 |
| Water | -0.244 | 2.1e-13 | 1.232 |
| pH | -0.241 | 3.8e-13 | 1.568 |
| Slope | 0.230 | 4.1e-12 | 1.651 |
| Road | -0.219 | 4.9e-11 | 2.273 |
| Aspect | -0.055 | 0.0998 | dropped, not significant |
| Elevation | 0.037 | 0.273 | dropped, not significant |

**Maximum VIF 2.608** (the paper reports 2.97 for Zn on its own nine
variables). The seven selected are *exactly* the set the `geosimilarity`
vignette uses — Slope, Water, NDVI, SOC, pH, Road, Mine — arrived at here from
the data rather than assumed. That is a nice point for the page: the vignette's
formula is not arbitrary, it is what the paper's own screen produces.

### The optimal similarity threshold (step 30, `results/kappa-rmse.csv`)

**lambda = 0.08**, cross-validation RMSE **0.6668**. At kappa = 1 (BCS) the
RMSE is **0.6729**, so choosing the threshold lowers cross-validation RMSE by
**0.91 %**. Only **8 % of the 885 observations** are used at each prediction
location. The curve has the paper's shape exactly: steep fall from kappa = 0.01
(RMSE 0.6944), a minimum at 0.08, then a slow rise to a plateau.

`gos_bestkappa()` seeds each repeat internally (1..nrepeat), so lambda is
deterministic regardless of the outer seed — worth saying on the page, and
tested in test-01.

### Grid prediction (step 40)

- GOS predictions of log Zn: mean 3.353, sd **0.365**, range 2.396-4.302.
- BCS predictions: mean 3.403, sd **0.138**, range 2.396-4.075.
- BCS spread is **38 %** of the GOS spread. **70.0 %** of BCS cells fall within
  +/-0.1 of the observed mean (3.439), against **12.8 %** of GOS cells — this
  is the paper's Fig. 10 finding, reproduced cleanly.

Mean prediction uncertainty (Eq. 12), GOS vs BCS:

| zeta | GOS | BCS | reduction by GOS |
|---|---|---|---|
| 0.9 | 0.0724 | 0.2203 | 67.1 % |
| 0.95 | 0.0545 | 0.1656 | 67.1 % |
| 0.99 | 0.0332 | 0.0801 | 58.5 % |
| 0.995 | 0.0295 | 0.0602 | 51.0 % |
| 0.999 | 0.0265 | 0.0351 | 24.5 % |
| 1 | 0.0258 | 0.0258 | 0 (identical, exactly) |

The paper reports 28.5-74.1 % reduction over zeta = 0.9 to 0.999; we get
**24.5-67.1 %**, and the identical-at-zeta-1 result holds to machine precision.

### Model comparison (step 50)

Three models, identical repeated 50/50 splits, errors in log Zn units.

**Primary run — outlier-screened, n = 885, lambda = 0.08, 50 repeats**
(`results/model-comparison.csv`):

| model | MAE | sd | RMSE | sd | MAE reduction by GOS | RMSE reduction by GOS |
|---|---|---|---|---|---|---|
| MLR | 0.5456 | 0.0124 | 0.6865 | 0.0154 | **5.81 %** | **4.18 %** |
| BCS | 0.5220 | 0.0125 | 0.6653 | 0.0174 | **1.54 %** | **1.12 %** |
| GOS | 0.5140 | 0.0119 | 0.6578 | 0.0183 | — | — |

**Robustness run — no outlier screening, n = 894, lambda re-derived = 0.08,
50 repeats** (`results/model-comparison-no-outlier-screening.csv`):

| model | MAE | sd | RMSE | sd | MAE reduction by GOS | RMSE reduction by GOS |
|---|---|---|---|---|---|---|
| MLR | 0.5640 | 0.0130 | 0.7184 | 0.0168 | 5.66 % | 3.69 % |
| BCS | 0.5401 | 0.0150 | 0.6994 | 0.0185 | 1.48 % | 1.08 % |
| GOS | 0.5321 | 0.0136 | 0.6919 | 0.0185 | — | — |

**RMSE ranking is GOS < BCS < MLR in both configurations.** Removing the
outlier screen shifts every error up by roughly 0.02-0.03 but changes neither
the ordering nor the size of GOS's advantage.

Paired over the shared splits, screened run:

- GOS beats MLR in **50/50** repeats on MAE (paired t, p = 1.3e-34) and 50/50
  on RMSE.
- GOS beats BCS in **50/50** repeats on MAE (paired t, p = 5.6e-20) and 47/50
  on RMSE.

Unscreened run: GOS beats MLR 50/50 on both; beats BCS 49/50 on MAE
(p = 5.9e-19) and 47/50 on RMSE.

This is the paper's core claim reproduced: using only the most similar 8 % of
observations beats using all of them (GOS > BCS), and both similarity models
beat the linear model.

### Runtime per step (`env/runtimes.csv`)

`10-prepare-data.R` 0.3 s; `20-select-variables.R` 0.0 s;
`30-best-kappa.R` 31.8 s; `40-gos-prediction.R` 10.0 s;
`50-model-comparison.R` 64.9 s; `90-tables.R` 0.0 s; **total 107.0 s**.
`Rscript run-all.R test` adds about 40 s.

---

## 3. Figures, with caption-ready sentences

Every figure exists as `figs/figNN-name.png` (196 dpi) and a matching vector
`figNN-name.pdf`. All are base R; Greek letters go through plotmath, so the
PDFs carry them as real Symbol-font text.

**`fig01-response-distribution`** — 4 panels.
> Distribution of the Zn response before and after preprocessing: histogram
> with kernel density (a) and normal Q-Q plot (b) of the 894 raw observations
> in ppm, and the same two views (c, d) of the 885 log-transformed values that
> remain after the outlier screen. The raw data are strongly right-skewed; the
> logarithm brings them close to normal (Shapiro-Wilk W = 0.986). Dashed lines
> mark the mean.

**`fig02-kappa-rmse`** — 2 panels.
> Determining the optimal similarity threshold. Cross-validation RMSE of log Zn
> against the percentage threshold kappa, averaged over 10 repeated 50/50
> splits, across the full candidate range (a) and zoomed to kappa <= 0.1 where
> the grid is finest (b). RMSE falls steeply while the first few percent of
> observations are added, reaches its minimum at lambda = 0.08 (open circle,
> dashed line; RMSE = 0.6668), then rises again as less similar observations
> begin contributing noise.

**`fig03-gos-prediction-uncertainty`** — 2 map panels with colour bars.
> GOS prediction of log Zn across the 13,132-cell 1 km grid at kappa = lambda =
> 0.08 (a) and the corresponding prediction uncertainty at zeta = 0.99 (b).
> Predictions run from 2.40 to 4.30 in log Zn; uncertainty is low over most of
> the study area (mean 0.033) and rises only where no observation has a similar
> geographical configuration. The uncertainty scale is clipped at its 99th
> percentile so the spatial pattern stays legible.

**`fig04-uncertainty-zeta`** — 6 map panels, one shared colour bar.
> GOS prediction uncertainty, Theta = 1 - Q(S_lambda, zeta), mapped at all six
> probability levels on a common scale. Uncertainty falls monotonically as zeta
> rises, from a mean of 0.072 at zeta = 0.9 to 0.026 at zeta = 1, while the
> spatial pattern of poorly matched locations stays in the same places.

**`fig05-bcs-vs-gos`** — a map panel and a density panel.
> Why the optimal threshold matters. The BCS prediction (a), which averages
> over every observation (kappa = 1), on the same colour scale as the GOS map
> in Fig. 3; and the distribution of all 13,132 predictions from both models
> (b). BCS collapses towards the study-area mean — sd 0.138 against 0.365 for
> GOS, with 70 % of its cells within +/-0.1 of the observed mean against 12.8 %
> for GOS — reproducing the concentration the paper reports in its Fig. 10.

**`fig06-model-comparison`** — 2 boxplot panels.
> Cross-validation errors of the three models over 50 repeated 50/50 splits of
> the 885 screened samples: mean absolute error (a) and root-mean-square error
> (b). Boxes show the spread across splits, white diamonds the means reported
> in the comparison table, and the dashed line the GOS mean. Every model is
> scored on identical splits. GOS improves on MLR on all 50 splits and on BCS
> on all 50 splits by MAE, lowering mean MAE by 5.8 % and 1.5 % respectively.

---

## 4. Where the numbers differ, and why

### 4a. Scope

Two things limit what can be compared with the published paper, and both
should be stated on the page.

1. **The packaged data are a subset.** `geosimilarity` bundles 894 Zn samples
   against the paper's 966, and carries only the nine natural and social
   covariates. The paper's Table 2 for Zn is led by two geological variables —
   *distance to Zn-related lithology* (R = -0.428) and *distance to Zn-related
   faults* (R = -0.350) — that are not in the packaged data at all. The
   strongest covariate available here is NDVI at R = -0.398. Published Table 3
   values therefore cannot reproduce digit-for-digit, and do not.
2. **This tutorial compares three models, the paper five.** The paper's Table 3
   also benchmarks GOS against ordinary kriging and regression kriging; those
   are out of scope here, so the paper's Table 3 comparison is not reproduced
   in full. The MLR, BCS and GOS rows are the ones being reproduced.

### 4b. Difference table

| what | this pipeline | source | why |
|---|---|---|---|
| Zn sample count | 894 (885 after screening) | paper: 966 | The packaged `zn` is a subset of the paper's data. |
| Zn summary stats | mean 42.55, median 29, max 353.5, sd 37.11, CV 0.872 | paper Table 1: mean 43.52, median 30, max 353.5, sigma 38.2, CV 0.88 | Same population, fewer samples. Max is identical. |
| Selected covariates | NDVI, SOC, Mine, Water, pH, Slope, Road | paper Table 2 for Zn: 9 variables led by two geological distances | The two geological variables are not in the packaged data. The overlapping variables agree closely: NDVI -0.398 vs -0.346, SOC -0.317 vs -0.312, Water -0.244 vs -0.259, pH -0.241 vs -0.232, Slope 0.230 vs 0.233, Road -0.219 vs -0.215. |
| Max VIF | 2.608 | paper: 2.97 for Zn | Fewer, less collinear variables. |
| lambda | 0.08 | paper: 0.04 for Zn | Same order of magnitude, same message ("only a few per cent of observations are needed"). The RMSE curve is very flat between kappa = 0.05 and 0.10 (0.6687 to 0.6673, a 0.2 % span), so the exact minimum is not sharply identified; with a coarser grid or different covariates it moves easily. |
| GOS vs BCS error reduction | MAE 1.54 %, RMSE 1.12 % | paper for Zn: MAE 2.6 %, RMSE 0.9 % | Same direction, same order of magnitude. |
| GOS vs MLR error reduction | MAE 5.81 %, RMSE 4.18 % | paper for Zn: MAE 6.0 %, RMSE 1.2 % | MAE almost exact; RMSE larger here. |
| Uncertainty reduction, GOS vs BCS | 24.5-67.1 % over zeta = 0.9-0.999 | paper: 28.5-74.1 % | Same magnitude and same direction. |
| Absolute error level | MLR RMSE 0.6865, GOS 0.6578 | paper for Zn: MLR 0.706, GOS 0.697 | Errors are somewhat lower here across the board; different sample set and covariates. |

Difference from the **package vignette**: none of substance. The vignette's
covariate set is reproduced exactly by the data-driven screen, and the vignette
does not report a model comparison. The vignette applies
`removeoutlier(coef = 2.5)`; the paper does not, which is why step 50 reports
both preprocessing variants (the ranking is identical either way).

### 4c. Implementation details worth a sentence on the page

- **`gos()`'s delta.** Eq. 4 defines `delta(u_alpha, v)` as the root mean
  square deviation of covariate X at all *unknown* locations from its value at
  observation location `u_alpha`. The package computes, for each *prediction*
  location, the sum of squared deviations from all *observation* locations
  divided by the number of prediction locations. This pipeline reproduces the
  package as shipped and does not alter it; `tests/test-01` transcribes Eqs.
  2-6 with the package's normalisation and matches `gos(kappa = 1)` to
  **0.0e+00**, so the min-operator composition and the Eq. 6 weighted mean are
  verified exactly.
- **Standardisation depends on the prediction set.** `gos()` computes each
  covariate's sigma over the observation *and* prediction locations together,
  so predicting a subset of the grid does not give the same numbers as
  predicting all of it. test-02 re-predicts all 13,132 cells for this reason.
- **Back-transformation is naive.** `pred_zn_ppm` is simply `exp(pred)`; no
  smearing or lognormal bias correction is applied. All errors in the
  comparison tables are in log Zn units, as in the paper.
- **No `car` dependency.** VIFs use `1/(1 - R^2)`, identical to `car::vif()`
  for continuous main effects; the pipeline cross-checks against `car` when it
  happens to be installed. Verified separately on 2026-08-25 with `car`
  installed: agreement on all seven covariates to within 5e-05 (the rounding
  of the CSV), and the values match the package vignette's `car::vif()` output.
- **Colour palettes.** Base R's `hcl.colors()` has no "Magma"; the prediction
  maps use reversed **"Rocket"** (the same magma family) and the uncertainty
  maps reversed **"Mako"**, both set in `config/project-config.R`.

---

## 5. Suggested reading order for the page

1. Data and preprocessing (Fig. 1, response-summary table).
2. Characterising the geographical configuration (variable-selection table) —
   note that the screen reproduces the vignette's covariate set exactly.
3. The optimal similarity threshold (Fig. 2, lambda = 0.08).
4. Prediction and uncertainty (Figs. 3 and 4).
5. Why the threshold matters (Fig. 5, GOS vs BCS).
6. Model comparison (Fig. 6, both comparison tables), followed by the
   reproduction-scope note from section 4a.
