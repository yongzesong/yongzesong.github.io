# Environment

- R >= 4.1
- Required: `sf`, `rpart`, `GD` (the geographical detector)
- Figures: `ggplot2`
- Optional, for the Step 2 demonstration only: `catchment`

```r
install.packages(c("sf", "rpart", "GD", "ggplot2"))

# The accessibility demonstration in Step 2 uses a GitHub-only package.
# Everything else runs without it.
install.packages("remotes")
remotes::install_github("uva-bi-sdad/catchment")
```

Verified run: R 4.6.0. The full pipeline takes about 15 s.
`env/session-info.txt` records the exact session.
