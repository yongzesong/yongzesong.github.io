# Environment

Everything on the tutorial page was produced by `Rscript run-all.R` in this
folder on **R 4.6.0**, single machine, no cluster.

Note: `requirements.md` is written by hand; everything else in `env/` —
`runtimes.csv` and `session-info.txt` — is written by the run itself.

## Packages

| package | used for |
|---|---|
| `sp` | coordinate handling for `gstat`/`automap` |
| `gstat` | unconditional Gaussian simulation (step 10), IDW benchmark |
| `automap` | `autoKrige` — the variogram fitting and ordinary kriging the paper used |
| `ranger` | the random-forest trend model, 500 trees |

Install line:

```r
install.packages(c("sp", "gstat", "automap", "ranger"))
```

Nothing else is required: the singularity indices, the SD filter, the spatial
block folds and the accuracy metrics are all base R in `R/02-srk-core.R`.
The figures are base graphics.

## Runtime

| step | seconds |
|---|---|
| `10-simulate.R` | 0.5 |
| `20-case-data.R` | 0.3 (plus one download of about 130 KB on first run) |
| `30-singularity.R` | 0.3 |
| `40-block-cv.R` | 7 |
| `50-seed-stability.R` | 85 |
| `60-sensitivity.R` | 55 |
| `90-tables.R` | < 0.1 |
| figures | about 4 |
| **total** | **about 150 s** |

`Rscript run-all.R test` adds about 8 s and runs 18 + 24 checks.

Steps 50 and 60 carry almost all of the runtime because each repeats the whole
five-fold cross-validation many times. Drop `SEED_REPEATS` in
`config/project-config.R` from 20 to 5, or shorten `SENS_MAX_SCALES`, and the
pipeline finishes in about half a minute.

## Scope

The article maps two trace elements; this pipeline follows **cobalt** end to
end. Every step is a loop over the `ELEMENTS` list in
`config/project-config.R`, so restoring zinc is one entry — the commented Zn
block in that file — and roughly doubles the runtime.

The pipeline stops at spatial block cross-validation. The published prediction
maps, cross-sections and uncertainty maps (article Figs. 9-11) need the
97,234-cell 500 m grid, which is not downloaded here; those figures appear on
the tutorial page as images from the article.

## Known version sensitivity

`automap::autoKrige` chooses the residual variogram family automatically, and
the choice can differ by one candidate between `automap`/`gstat` versions. On
this machine that shows up on two of the five ordinary-kriging folds for Co:

| fold | this machine | authors' released `cv_detail_Co.csv` |
|---|---|---|
| 1 | R2 0.158461, RMSE 14.1016 | identical |
| 2 | R2 0.246547, RMSE 9.2553 | R2 0.241386, RMSE 9.2869 |
| 3 | R2 0.189150, RMSE 11.6603 | R2 0.159638, RMSE 11.8706 |
| 4 | R2 0.027565, RMSE 12.2063 | identical |
| 5 | R2 0.137348, RMSE 12.8719 | identical |

That moves the OK fold mean from their 0.1449 to 0.1518 here, against 0.148 in
the article. The same mechanism nudges SRK, whose residual-kriging step is also
an `autoKrige` call; it is part of why the authors' released SRK value (0.344)
sits below the range this pipeline produces over 20 forest seeds
(0.352-0.362).

The two benchmarks that involve neither kriging nor a forest — IDW and LM —
reproduce the authors' fold-level values to 1e-6 on all five folds, which is
what `tests/test-02` asserts.
