# =============================================================================
# 02-response-intake.R — register the observed response under the standard name
#
# The analysis table carries the observed response (config: response$direct_col,
# default "y"). This step copies it to the pipeline's standard internal name
# (response$response_name, default "z") together with the unit id and
# coordinates, so every later step reads the response from one file.
#
# Output: results/step02-response.csv
# =============================================================================

source("R/functions/fn-config.R")

cfg <- load_config()
ensure_dir("results")

d    <- read_analysis_table(cfg)
resp <- cfg$response$response_name

out <- d[, c(cfg$data$id_col, cfg$data$x_col, cfg$data$y_col)]
out[[resp]] <- d[[cfg$response$direct_col]]

cat(sprintf("Direct response '%s' copied to '%s'.\n",
            cfg$response$direct_col, resp))
cat(sprintf("Response %s: mean=%.4f  sd=%.4f  min=%.4f  max=%.4f\n",
            resp, mean(out[[resp]]), sd(out[[resp]]),
            min(out[[resp]]), max(out[[resp]])))

write.csv(out, "results/step02-response.csv", row.names = FALSE)
cat("Saved: results/step02-response.csv\n")
