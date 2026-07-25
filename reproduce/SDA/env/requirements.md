# Environment

The method itself (`R/02-sda-core.R`) uses **base R only** — no packages at all.
The pipeline around it uses a few, all optional in the sense that the step
degrades with a message rather than failing:

- `ggplot2` (+ `ggrepel`) — the five figures
- `randomForest` — the random-forest arm of the cross validation
- `geosphere` — used for distances when present; the built-in haversine in
  `02-sda-core.R` is used otherwise and agrees to 1e-6 m (test-01 checks this)

```r
install.packages(c("ggplot2", "ggrepel", "randomForest", "geosphere"))
```

Verified run: R 4.6.0. The full pipeline takes about 6 s on the demo data.
`env/session-info.txt` records the exact session.
