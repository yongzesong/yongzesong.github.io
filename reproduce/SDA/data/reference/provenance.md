# Data provenance

The demo reproduces the geochemical case of Song (2022),
doi:10.1016/j.jag.2022.102834 — trace elements in soil, with the geographical
environment described by eight surfaces.

| File | Contents |
|---|---|
| `../obs.csv` | 614 sample locations: `Lon`, `Lat`, `Cr_ppm`, `Cu_ppm` |
| `../grids.csv` | 68,757 grid cells: `Lon`, `Lat`, `Elevation` |
| `../sample-vars-fda.csv` | first-dimension variables: the eight surfaces read at the sample locations |
| `sda-<surface>.csv` | published second-dimension variables, 55 columns named `b<b>t<tau>` for b in {1,3,5,7,9} km and tau in {0, 0.1, ..., 1} |

The values are those distributed with the method in the `SecDim` package
(Song, CRAN). `R/20-generate-sdvars.R` regenerates `sda-elevation.csv` from
`obs.csv` and `grids.csv` and `tests/test-01` checks the result is identical,
so the generator is verified against the published values rather than trusted.

The remaining seven surfaces are supplied in generated form because the
published grid file carries `Elevation` only.
