# Data provenance

`econineq.gpkg` — 333 Statistical Area Level 3 (SA3) regions of Australia with
the Gini coefficient of income inequality and eight explanatory variables
(industry scale, IT employment, income, sex ratio, home ownership, industrial
employees, industrial companies, higher education).

The file is the one distributed with the **geocomplexity** R package
(`system.file("extdata/econineq.gpkg", package = "geocomplexity")`) and is
copied here so the folder runs on its own. It is the case study of:

> Zhang Z, Song Y, Luo P, Wu P (2023). Geocomplexity explains spatial errors.
> *International Journal of Geographical Information Science* 37(7):1449-1469.
> doi:10.1080/13658816.2023.2203212 (open access)

Note on comparability: this layer carries all eight explanatory variables, and
the published article reports a variable-selection step before modelling. The
absolute fit statistics here therefore differ from the article's Tables 3-4,
while the mechanism and the ordering reproduce. See section 3.2 of the tutorial.
