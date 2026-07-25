# Data provenance

The demo runs on the built-in datasets of the **GD** R package (Song et al.),
loaded by name in `config/project-config.R` (`DATASET <- "ndvi_40"`):

- `ndvi_5 … ndvi_50` — annual mean NDVI change (2010–2014), Inner Mongolia,
  China, aggregated to 5–50 km grid cells. Response `NDVIchange`; explanatory
  variables `Climatezone`, `Mining` (categorical) and `Tempchange`,
  `Precipitation`, `GDP`, `Popdensity` (continuous). Source: Song, Y., Wang, J.,
  Ge, Y. & Xu, C. (2020), GIScience & Remote Sensing 57(5):593–610.
- `h1n1_50/100/150` — H1N1 flu incidence, an alternative built-in case.

No external download is needed: `install.packages("GD")` ships the data.
To use your own table instead, set `INPUT_FILE` in the config to a CSV path.
