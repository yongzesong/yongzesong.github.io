# Environment

## Software

- R >= 4.1 (verified on **R 4.6.0**, aarch64-apple-darwin23)
- The method: `geosimilarity` **3.9** — supplies `gos()`, `gos_bestkappa()`,
  `removeoutlier()` and both bundled data sets
- Everything else is base R. Figures use base graphics only; no ggplot2 is
  used anywhere in this pipeline (the ggplot object returned in
  `gos_bestkappa()$plot` is ignored).

```r
install.packages("geosimilarity")
```

One package is optional:

- `car` — if installed, `R/20-select-variables.R` cross-checks its `vif()`
  against the base-R VIFs the pipeline computes. Nothing depends on it:
  `vif_lm()` in `R/01-helpers.R` uses the identity `VIF_i = 1 / (1 - R2_i)`,
  which is exactly what `car::vif()` returns for a model with continuous main
  effects only. The cross-check was run separately on 2026-08-25 with
  `car` 3.1-x installed: the two agree on all seven selected covariates to
  within 5e-05, which is the rounding of `results/variable-selection.csv`
  (Slope 1.6510, Water 1.2325, NDVI 1.4595, SOC 1.3558, pH 1.5683,
  Road 2.2734, Mine 2.6083 — the same values the package vignette reports).

`env/session-info.txt` records the exact session of the verified run.

## Running it

```r
Rscript run-all.R            # the whole pipeline
Rscript run-all.R 30 40      # only the named steps
Rscript run-all.R test       # only the two verification scripts
```

Steps read what earlier steps wrote, so a single step can be re-run after a
change in `config/project-config.R` without repeating the rest. A partial run
updates only its own rows in `env/runtimes.csv`.

## Measured runtime

From `env/runtimes.csv`, one core, R 4.6.0 on an Apple-silicon laptop:

| step | seconds |
|---|---|
| `10-prepare-data.R` | 0.3 |
| `20-select-variables.R` | 0.0 |
| `30-best-kappa.R` | 31.8 |
| `40-gos-prediction.R` | 10.0 |
| `50-model-comparison.R` | 64.9 |
| `90-tables.R` | 0.0 |
| **total** | **107.0** |

Almost all of it is cross-validation: step 30 fits 19 candidate kappa values x
10 repeats, and step 50 runs 3 models x 50 repeats **twice** (once on the
outlier-screened samples, once on all 894 — see below), with a second
`gos_bestkappa()` search in between. The two full-grid `gos()` calls in step 40
take about 4.5 s and 5.0 s. `Rscript run-all.R test` adds about 40 s, because
test-02 re-derives lambda and re-predicts the whole grid from scratch.

`CV_REPEATS` in `config/project-config.R` is the knob to turn if this needs to
be faster; the paper's own value of 50 was affordable here, so it was kept.

## Scope of the reproduction — read this before quoting numbers

The pipeline uses the two data sets bundled with `geosimilarity` and downloads
nothing: `zn` (894 samples x 12 columns) and `grid` (13,132 prediction cells at
1 km). This is the paper's Zn subset for the Leonora mining region, Western
Australia. It is **not** the full data set behind the published tables, in two
ways that matter:

1. **The geological covariates are missing.** The paper's Table 2 selects nine
   variables for Zn, and the two strongest are *distance to Zn-related
   lithology* (R = -0.428) and *distance to Zn-related faults* (R = -0.350).
   Neither is in the packaged data, which carry only the nine natural and
   social covariates (Elevation, Slope, Aspect, Water, NDVI, SOC, pH, Road,
   Mine). The strongest covariate available here is NDVI at R = -0.398.
2. **n = 894, not the paper's 966 Zn samples.**

So the published Table 3 values for Cu, Zn and Pb are **not expected to
reproduce digit-for-digit**, and they do not. What this tutorial reproduces is
the *method* — the correlation-and-VIF screen, the kappa search of Eqs. 7-9,
the Eq. 11 prediction, the Eq. 12 uncertainty at all six zeta levels — and the
*pattern* of the paper's findings.

A third difference is one of scope rather than of data: the paper's Table 3
benchmarks GOS against five models, including ordinary kriging and regression
kriging. This tutorial compares **MLR, BCS and GOS only**, so the paper's
Table 3 comparison is not reproduced in full.

`NOTES-for-tutorial.md` lists every place the numbers differ from the paper or
from the package vignette, with the reason.

## Choices worth knowing

- **Two preprocessing variants are reported.** The package vignette applies
  `removeoutlier(coef = 2.5)`; the paper explicitly does not remove high trace-
  element values (Sect. 3.2: they "may indicate the clusters of mineral
  deposits"). Step 50 runs the model comparison both ways, re-deriving lambda
  for the unscreened data so GOS is not handicapped. The ranking is unchanged.
- **PNG back end.** The cairo build of R on this machine cannot load its X11
  dependencies, so `png()` is left on R's default (`quartz`) rather than being
  forced to `type = "cairo"`, which silently produced no file. `draw_figure()`
  errors out if a device writes nothing.

## A note on this file

`env/` is written to by the pipeline (`session-info.txt`, `runtimes.csv`) but
this document and `data/provenance.md` are hand-maintained and are **not**
regenerated by `run-all.R`. Do not clear those directories wholesale.
