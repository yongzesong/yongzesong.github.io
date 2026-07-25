# Environment

The method itself (`R/02-stor-core.R`) uses **base R only** — standardization,
entropy weights, the LOESS utility (via `stats::loess`), local Moran's I and
the GWR are all implemented in that one file. The pipeline around it uses:

- `mgcv` — the GAM of the income step (ships with every R installation)
- `ggplot2` (+ `patchwork`) — the five figures; each figure script degrades
  with a message rather than failing when they are absent
- `spdep` — **tests only**: cross-checks the built-in local Moran's I against
  `spdep::localmoran()` (agreement to 1e-9; skipped when not installed)

```r
install.packages(c("ggplot2", "patchwork", "spdep"))
```

Verified run: R 4.6.0. The full pipeline takes about 20 s on the simulated
1600-block data (the LISA permutations and the GWR dominate). `env/session-info.txt`
records the exact session.
