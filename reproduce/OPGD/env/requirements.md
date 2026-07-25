# Environment

- R >= 4.1
- Packages: `GD` (the OPGD reference implementation; ships the demo data),
  `ggplot2` (figures).

```r
install.packages(c("GD", "ggplot2"))
```

Verified run: R 4.6.0, GD 10.9. The full pipeline runs in ~2–3 s on the
built-in `ndvi_40` dataset. `env/session-info.txt` records the exact session.
