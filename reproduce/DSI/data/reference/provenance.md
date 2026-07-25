# data/reference — provenance

Files here are **not** project data. They are the published reference material that `tests/test-01-reproduce-dsi-paper.R` checks the implementation against. Do not modify or delete them; the test is the only guarantee that this template computes the published method.

## dsi-paper-demo-dataset.csv

The demo dataset released with the DSI paper, obtained from <https://github.com/KevinOceanLiu/DSI>.

586 rows, 23 columns. Derived from the Australian case study of Liu et al. (2026): the held-out test set at 0.5° × 0.5° grid resolution, carrying WGS84 coordinates, the response `y` (vascular plant species richness), the `residual` column from the paper's linear regression model, and the same 19 environmental, topographic and soil predictors reported in the article.

Being the exact input behind the paper's Moran's I and Q value computations, it reproduces the published θ_min, θ_probable and θ_max for the `lm` row.

## dsi-paper-original-code.R

The reference implementation, unmodified, from the same repository. Kept for comparison: the template's `R/02-spatial-metrics.R` and `R/03-dsi.R` restructure this code into functions and drop the `GD` dependency by implementing the Q statistic directly, but compute the same quantities.

Two differences worth knowing when reading the two side by side. The original calls `GD::gd()` for the Q value, while the template implements the sum-of-squares form directly, which reproduces the published numbers exactly and removes a dependency. And the original reads a single CSV with a pre-computed `residual` column, while the template fits the models itself and produces one residual column per model.

## Values reproduced

From Table 3 (row `lm`) and Figure 6 of Liu et al. (2026):

| Quantity | Published | Computed by this template |
|---|---|---|
| δᵃ₀ | 0.409 | 0.410 |
| δᵃᵣ | 0.189 | 0.190 |
| δʰ₀ | 0.513 | 0.513 |
| δʰᵣ | 0.203 | 0.203 |
| ηᵃ | 0.538 | 0.538 |
| ηʰ | 0.604 | 0.605 |
| θ_min | 0.538 | 0.538 |
| θ_probable | 0.604 | 0.605 |
| θ_max | 0.817 | 0.817 |

The Moran's I differences of 0.001 are rounding in the published table. Test tolerance is 0.005.

## Citation

Liu, H., Song, Y., & Yi, W. (2026). Degree of spatial interpretability. *International Journal of Geographical Information Science*. <https://doi.org/10.1080/13658816.2026.2614335>

Data and code for the source study: <https://doi.org/10.6084/m9.figshare.27959079>
