# Requirements

R 4.1 or later. Verified on R 4.6.0, macOS.

## Required

The pipeline stops without these.

```r
install.packages(c("sf", "spdep", "rpart"))
```

| Package | Used for |
|---|---|
| `sf` | projecting coordinates to a metric CRS |
| `spdep` | k-nearest-neighbour weights, Moran's I and its significance test |
| `rpart` | the regression tree that defines the Q-value strata |

## Optional, by what each one enables

Missing packages disable one model or one module, with a message naming the package. The pipeline always produces output.

```r
# the local DG module
install.packages("geocomplexity")

# figures
install.packages(c("ggplot2", "scales", "patchwork"))

# the model set of the published papers
install.packages(c("ranger", "xgboost", "mgcv", "Cubist", "FNN",
                   "pls", "earth", "kernlab", "gbm"))
```

| Package | Enables | Without it |
|---|---|---|
| `geocomplexity` | the whole DG module | step 4 skips; global DSI unaffected |
| `ggplot2` | every figure | steps 1–6 still write results and tables |
| `scales`, `patchwork` | DG map colour limits, combined accuracy panel | those two figures skip |
| `ranger` | random forest | model `rf` skipped |
| `xgboost` | gradient boosting | model `xgbTree` skipped |
| `mgcv` | generalised additive model | model `gam` skipped |
| `Cubist` | rule-based model | model `cubist` skipped |
| `FNN` | k-nearest neighbours | model `knn` skipped |
| `pls`, `earth`, `kernlab`, `gbm` | PLS, MARS, SVM, GBM | those models skipped |

`sf` needs GDAL, GEOS and PROJ. On macOS, `brew install gdal geos proj`; on Debian or Ubuntu, `apt install libgdal-dev libgeos-dev libproj-dev libudunits2-dev`.

## Recording the environment

`R/60-tables.R` writes `env/session-info.txt` on every run, capturing R version, platform and the version of every loaded package. Include it in the data deposit — it is what lets a reader reconstruct the environment your numbers came from.

## Pinning versions

For a long-running project, `renv` freezes package versions:

```r
install.packages("renv")
renv::init()      # in the project root, after the first successful run
renv::snapshot()  # writes renv.lock
```

A collaborator then runs `renv::restore()`. The template does not ship a lockfile, because one generated on a different platform causes more trouble than it prevents.

## Manuscript

TeX Live or MacTeX, with `natbib`, `booktabs`, `graphicx`, `amsmath`, `lineno`, `setspace`, `hyperref` and `xcolor` — all in a standard full installation.

```bash
cd manuscript && latexmk -pdf 01-main.tex
```
