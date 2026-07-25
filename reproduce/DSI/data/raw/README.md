# data/raw

Your input data goes here, unmodified from its source. Nothing in this folder is written by the pipeline.

## Input contract

One table, one row per sampling location, CSV or any format you convert to CSV.

| Column | Requirement |
|---|---|
| `lon` | longitude or easting, in the CRS declared as `DATA_CRS` |
| `lat` | latitude or northing, same CRS |
| *response* | numeric; name it in `RESPONSE` in `config/project-config.R` |
| *predictors* | numeric; every remaining column, unless listed in `DROP_COLS` |

Predictors serve two purposes: they fit the models, and they define the strata for the Q value. A column that is an identifier rather than a covariate must go in `DROP_COLS`, or it will end up partitioning space.

## What step 1 does to it

`R/10-prepare-data.R` reads this folder, drops rows with missing coordinates, response or predictors, drops duplicate coordinates, optionally log-transforms the response, reports predictor pairs correlated above 0.8 without acting on them, and writes `data/derived/analysis-data.csv`. Everything downstream reads only the derived file, so your raw data is never modified.

## Recording the source

Add one line per dataset here, so the availability statement can be written from the record rather than from memory:

| File | Source | Version or access date | Licence |
|---|---|---|---|
| | | | |
