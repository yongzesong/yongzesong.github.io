# Environment

- R >= 4.1
- Required: `sf`, `sdsfun`, `geocomplexity` (the method), `spdep`
- Models: `e1071` (SVR), `GWmodel` (GWR and the error explanation)
- Figures: `ggplot2`, `scales`

```r
install.packages(c("sf", "sdsfun", "geocomplexity", "spdep",
                   "e1071", "GWmodel", "ggplot2", "scales"))
```

Verified run: R 4.6.0, geocomplexity 0.3.0. The full pipeline takes about
90 s, nearly all of it in the GWR bandwidth searches (five of them).
`env/session-info.txt` records the exact session.
