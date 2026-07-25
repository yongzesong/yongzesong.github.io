/**
 * Google Earth Engine — covariate extraction to the SOH analysis grid
 * ---------------------------------------------------------------------------
 * Adapted from the pattern used in
 * github.com/renkaigis/Second-dimension_Outlier-driven_Heterogeneity (MIT).
 *
 * Produces the `grids.csv` that 01_prepare_data.R expects: one row per grid
 * cell with x, y and one column per covariate.
 *
 * Three rules that matter more here than in an ordinary GEE export.
 *
 * 1. Export in a PROJECTED CRS with metric units. SOH buffers are distances,
 *    and degrees make every radius wrong by a latitude-dependent factor.
 * 2. Buffer the study region by at least the largest SOH radius before
 *    clipping. Units near the boundary otherwise get truncated neighbourhoods
 *    and systematically weaker outlier patterns.
 * 3. Export the grid FINER than the analysis units. A covariate exported at
 *    unit resolution has no second dimension to extract.
 * ---------------------------------------------------------------------------
 */

// --- 1. Study region and grid ----------------------------------------------

var studyArea = ee.FeatureCollection('users/YOUR_USER/YOUR_STUDY_AREA');

var MAX_BUFFER_M = 200000;   // largest SOH radius, in metres
var GRID_RES_M   = 1000;     // analysis grid resolution, in metres
var CRS          = 'EPSG:3577';  // projected CRS with metric units

var region = studyArea.geometry().buffer(MAX_BUFFER_M);

// --- 2. Covariates ----------------------------------------------------------
// Replace with the drivers of your response. Group them so the categories in
// config/config.R are meaningful to a domain reader.

var START = '2023-01-01';
var END   = '2024-01-01';

// Category C1: climate
var era5 = ee.ImageCollection('ECMWF/ERA5_LAND/DAILY_AGGR')
  .filterDate(START, END)
  .filterBounds(region);

var AT = era5.select('temperature_2m').mean().rename('AT');
var TP = era5.select('total_precipitation_sum').sum().rename('TP');

// Category C2: topography
var ELE = ee.Image('AU/GA/DEM_1SEC/v10/DEM-S').select('elevation').rename('ELE');

// Category C3: vegetation
var NDVI = ee.ImageCollection('MODIS/061/MOD13A2')
  .filterDate(START, END)
  .select('NDVI').mean().multiply(0.0001).rename('NDVI');

// --- 3. Stack ---------------------------------------------------------------

var stack = AT.addBands(TP).addBands(ELE).addBands(NDVI)
  .clip(region)
  .reproject({crs: CRS, scale: GRID_RES_M});

// --- 4. Sample to a point per grid cell -------------------------------------
// geometries: true keeps coordinates, which become the x and y columns.

var samples = stack.sample({
  region: region,
  scale: GRID_RES_M,
  projection: CRS,
  geometries: true,
  dropNulls: true,
  tileScale: 4
});

// Attach projected coordinates as explicit columns.
var withXY = samples.map(function (f) {
  var c = f.geometry().transform(CRS, 1).coordinates();
  return f.set({
    x: ee.Number(c.get(0)).divide(1000),   // metres to kilometres
    y: ee.Number(c.get(1)).divide(1000)
  });
});

// --- 5. Export --------------------------------------------------------------

Export.table.toDrive({
  collection: withXY,
  description: 'soh_grids',
  fileNamePrefix: 'grids',
  fileFormat: 'CSV',
  selectors: ['x', 'y', 'AT', 'TP', 'ELE', 'NDVI']
});

// --- 6. Checks before exporting ---------------------------------------------
print('grid cells:', withXY.size());   // keep under a few hundred thousand
print('first row:', withXY.first());
Map.centerObject(studyArea, 6);
Map.addLayer(stack.select('NDVI'), {min: 0, max: 0.8}, 'NDVI');
Map.addLayer(studyArea, {color: 'red'}, 'study area');

/**
 * After the export finishes:
 *   1. Put grids.csv in data/processed/
 *   2. Build points.csv the same way, sampling at your analysis unit centroids
 *      or aggregating the grid within unit polygons, and join the response
 *   3. Set cfg$data$mode to "user" in config/config.R
 *   4. Run code/99_run_all.R
 *
 * A grid over roughly a hundred thousand cells makes SOP generation slow.
 * Coarsen GRID_RES_M or set cfg$sop$max_cells before assuming the code hung.
 */
