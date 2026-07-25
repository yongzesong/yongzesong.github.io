# tests

```bash
Rscript run-all.R test
```

Both tests exit non-zero on failure, so they work in a pre-commit hook or CI.

## test-01-reproduce-dsi-paper.R

Recomputes the values printed in Table 3 (row `lm`) and Figure 6 of Liu et al. (2026) from the demo dataset released with the paper, and compares all nine against the published numbers at a tolerance of 0.005.

Run it after any change to `R/02-spatial-metrics.R` or `R/03-dsi.R`, and before trusting any number the pipeline produces on your own data. A failure means the implementation has drifted from the published method — which is the failure mode that otherwise goes unnoticed, because a wrong indicator still produces plausible-looking output.

When it fails, check in order: `PROJECTED_CRS` is 3577, k is 8, the Q statistic still uses the sum-of-squares form, and the strata still come from a regression tree fitted to `y` on all predictors.

## test-02-metric-properties.R

Seventeen checks on behaviour that follows from the definitions rather than from data: η equals 1 when the residual field is unstructured and 0 when it equals the response, η is negative when the model amplifies structure, θ_max exceeds every component, the Q statistic approaches 1 for separated strata and 0 for random ones and is invariant to rescaling, Moran's I is high for a structured field and near zero for the same values shuffled, and a fitted model beats a mean-only model on the synthetic case.

The last two checks are the ones that catch a broken port: if a fitted model does not outperform predicting the mean everywhere, the indicator is not tracking spatial explanation on your data.

## Adding a test

Both files use a local `check(label, condition)` helper and need no test framework. Add a case where the correct answer is known independently of the code — from the paper, from a definition, or from a construction where the answer is obvious.
