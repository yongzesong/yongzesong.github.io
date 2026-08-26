# Reproducing Singularity Regression Kriging (SRK)

Companion code for the tutorial page
<https://yongzesong.com/reproduce/SRK/srk.html>.

Method: Ren K, Song Y, Chen M, Yu Q (2026). A singularity regression kriging
for spatial prediction. *GIScience & Remote Sensing* 63(1):2690341.
<https://doi.org/10.1080/15481603.2026.2690341>

This pipeline is a port of the authors' released scripts
(<https://github.com/renkaigis/Singularity_Regression_Kriging>) into the
step-numbered layout used by the other tutorials in this series.

## Run it

```r
install.packages(c("sp", "gstat", "automap", "ranger"))
```

```
Rscript run-all.R          # full pipeline, about 150 seconds
Rscript run-all.R test     # 18 + 24 verification checks, about 8 seconds
Rscript run-all.R 10 40    # only the named steps
Rscript run-all.R figures  # only the figure scripts
```

Step 20 downloads the case-study sample table (about 130 KB) from the authors'
repository on first run. It is not redistributed here; see
`data/provenance.md` for its original sources.

## Layout

```
run-all.R                   one command runs everything
config/project-config.R     the only file you edit to port this elsewhere
R/02-srk-core.R             the method: singularity, RF trend, residual kriging
R/10-simulate.R             paper Sec. 2.3 — Table 1, Figs. 1-2
R/20-case-data.R            download, tidy, Spearman screen (Sec. 4.1-4.2)
R/30-singularity.R          covariate singularity features (Sec. 4.3)
R/40-block-cv.R             six models, 5-fold spatial block CV (Table 2)
R/50-seed-stability.R       how much of the SRK-RFK gap is forest noise
R/60-sensitivity.R          scale x threshold grid (Fig. 7)
R/90-tables.R               LaTeX tables
R/p0*.R                     figures
tests/                      method properties, then reproduction of the paper
```

## Scope

Simulation experiment and case study up to cross-validation. The article maps
two trace elements; this pipeline follows **cobalt** end to end. Every step is a
loop over the `ELEMENTS` list in `config/project-config.R`, so restoring zinc is
one entry — the commented Zn block in that file.

The published full-grid prediction maps, cross-sections and uncertainty maps are
out of scope and are shown on the tutorial page as images from the article.
