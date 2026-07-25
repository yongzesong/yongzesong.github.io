# Data provenance

The case is greenspace access and accessibility across metropolitan Perth,
Western Australia, from:

> Lin G, Song Y, Xu D, Swapan MSH, Wu P, Hou W, Xiao Z (2024). Interpreting
> differences in access and accessibility to urban greenspace through
> geospatial analysis. *International Journal of Applied Earth Observation and
> Geoinformation* 129:103823. doi:10.1016/j.jag.2024.103823 (open access, CC BY)

| File | Contents |
|---|---|
| `mbvars.csv` | 8,259 residential mesh blocks. `u3…u30` standardised access (α), `v3…v30` standardised accessibility (β), `df3…df30` their difference (δ), at walking times of 3, 5, 10, 15, 20 and 30 minutes. Six geospatial variables (`popdenskm2`, `dwedenskm2`, `shapefacto`, `compactrat`, `neargsdist`, `neargsarea`) and their contextualised forms (`lisav1…lisav6`), plus the SA3 region. |
| `blocks.gpkg` | 9,449 mesh-block polygons: 8,276 residential (the consumers) and 1,173 parkland (the greenspace providers), with population, dwellings and area. |
| `regions.gpkg` | the nine SA3 regions of the study area. |

`mbvars.csv` holds the authors' own α and β, so the results in sections 3.3
onward are the published ones. The identifier `MB_CODE21` is an 11-digit code
and is stored as text: read as a number it loses precision and stops matching
the geometry.

**On recomputing α and β.** Step 2 of the pipeline recomputes both from
`blocks.gpkg` to show how they are produced, using straight-line distances. The
published β used road-network distances built in QGIS (see the tutorial), so the
demonstration reproduces the tutorial's own printed figures rather than the
published columns — access correlates at r ≈ 0.61 and accessibility at r ≈ −0.08.
Section 3.2 of the tutorial page sets this out in full.
