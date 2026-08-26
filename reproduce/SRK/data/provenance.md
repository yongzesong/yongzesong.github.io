# Data provenance

The sample table below is downloaded by `R/20-case-data.R` from the code
repository released with the paper and is **not** redistributed with this
tutorial:

    https://github.com/renkaigis/Singularity_Regression_Kriging

| file | rows | what it holds |
|---|---|---|
| `dt_Co_linear.csv` | 998 | Co samples: coordinates, Co concentration, four truncated-linear lithology proximity variables (ss, ifi, imi, hm) and three DEM terrain variables |

## Original sources

* Geochemistry: GSWA Geochemistry database (DMIRS-047), Department of Energy,
  Mines, Industry Regulation and Safety, Western Australia.
* Lithology: Surface Geology of Australia, Geoscience Australia (2012), turned
  into per-unit proximity variables by the truncated linear decay of Eq. 14
  (1 inside the unit, falling to 0 over a 10 km buffer).
* Terrain: elevation, slope and aspect from a Geoscience Australia DEM (2015).

## Coordinates

`x` and `y` are Web Mercator metres (EPSG:3857) near 122.5E, 32.5S. At that
latitude Web Mercator inflates distance by 1/cos(32.5 deg) = 1.19, so the
2-20 km singularity scales and the 15 km cross-validation block are about
1.7-16.9 km and 12.6 km on the ground. Every distance in this pipeline is
expressed in the same projected metres as the published analysis.

## Not downloaded

`grid_linear.csv` (97,234 cells at 500 m) drives the published prediction
maps. This tutorial stops at cross-validation, so the grid is not needed;
the maps are shown on the page as figures from the article.
