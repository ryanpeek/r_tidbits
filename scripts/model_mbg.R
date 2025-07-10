# MBG
#https://henryspatialanalysis.github.io/mbg/
# need INLA
# https://www.r-inla.org/download-install
# Load packages ---------

library(data.table)
library(ggplot2)
library(sf)
library(terra)
library(mbg)

#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

library(INLA)
inla.upgrade()


# Data --------------------------------------------------------------------

# Outcome: child stunting
outcomes <- data.table::fread(
  system.file('extdata/child_stunting.csv', package = 'mbg')
)

communes <- sf::st_read(
  system.file('extdata/Benin_communes.gpkg', package = 'mbg'),
  quiet = TRUE
)

# Convert point data to sf
outcome_sf <- sf::st_as_sf(
  outcomes,
  coords = c('x', 'y'),
  crs = sf::st_crs(communes)
)
outcome_sf$stunting_rate <- outcome_sf$indicator / outcome_sf$samplesize

ggplot2::ggplot() +
  ggplot2::geom_sf(data = communes) +
  ggplot2::geom_sf(
    data = outcome_sf,
    ggplot2::aes(color = stunting_rate, size = samplesize),
    alpha = 0.75
  ) +
  ggplot2::scale_size_continuous(range = c(0.5, 3)) +
  ggplot2::scale_color_gradientn(
    colors = terra::map.pal('viridis'),
    labels = scales::percent
  ) +
  ggplot2::labs(
    title = 'Stunting point data',
    color = 'Observed\nstunting\nrate',
    size = 'Sample\nsize'
  ) +
  ggplot2::theme_minimal()


# Covariates --------------------------------------------------------------

# Spatial covariates
covariates <- list(
  access = terra::rast(system.file('extdata/access.tif', package = 'mbg')),
  evi = terra::rast(system.file('extdata/evi.tif', package = 'mbg')),
  temperature = terra::rast(system.file('extdata/temperature.tif', package = 'mbg'))
)

summary(covariates)

# Plot the covariates
plot(terra::rast(covariates), nr = 1)


# Make a Model ------------------------------------------------------------

# A Bayesian geostatistical model can accomplish all of these goals. We will fit a Bayesian geostatistical model to the outcome data, with three potential predictors formatted as raster surfaces:

# Travel time to the nearest city, in minutes
# Enhanced vegetation index (EVI)
# Mean annual temperature


# Create ID raster: lays out the prediction grid. The model will make predictions for all non-NA cells in the id_raster
id_raster <- mbg::build_id_raster(
  polygons = communes,
  template_raster = covariates[[1]]
)
# Table to help with aggregation to higher administrative levels, used to aggregate model estimates from grid cells to commune polygons, preserving uncertainty
aggregation_table <- mbg::build_aggregation_table(
  polygons = communes,
  id_raster = id_raster,
  polygon_id_field = 'commune_code'
)
# Population raster: used for aggregation to administrative boundaries (population-based indicators like child stunting should be aggregated using population weighting, which places greater weight on grid cells with higher populations when summarizing those grid cell results by polygon.)
population_raster <- terra::rast(
  system.file('extdata/under_5_population.tif', package = 'mbg')
)


# Run Model ---------------------------------------------------------------

# Add an intercept to the model
# want a regression intercept in this model, the first line in the code block below creates the intercept as one of the covariate rasters.

covariates$intercept <- covariates[[1]] * 0 + 1
## Run MBG models
model_runner <- MbgModelRunner$new(
  input_data = outcomes,
  id_raster = id_raster,
  covariate_rasters = covariates,
  aggregation_table = aggregation_table,
  aggregation_levels = list(
    commune = c('commune_code', 'commune', 'department_code', 'department'),
    region = c('department_code', 'department')
  ),
  population_raster = population_raster
)
model_runner$run_mbg_pipeline()


# grid cell predictions:
# model makes predictions at each grid cell, with some uncertainty associated with each grid cell prediction. We capture that uncertainty by taking many (by default, 250) samples from the range of plausible model estimates and generating summary statistics across those samples.

# Get predictions by pixel
grid_cell_predictions <- model_runner$grid_cell_predictions
# Plot mean estimates
plot(
  grid_cell_predictions$cell_pred_mean * 100,
  main = 'MBG mean estimates (%)'
)
lines(communes)

# show 95% uncertainty interval by subtracting the “lower” from the “upper” summary raster:
# Plot estimate uncertainty
plot(
  (grid_cell_predictions$cell_pred_upper - grid_cell_predictions$cell_pred_lower) * 100,
  col = sf::sf.colors(n = 100),
  main = 'MBG estimates: 95% uncertainty interval width (%)'
)
lines(communes)

# Aggregated samples and summaries are available from the MbgModelRunner$aggregated_predictions attribute. This attribute is a named list, where each item contains polygon estimates for each of the passed aggregation_levels. Each of those levels has two items:

##  'draws' (data.table::data.table): Predictive model samples by aggregated polygon unit
## 'summary' (data.table::data.table): Summaries of draws for each aggregated polygon unit, including the mean and the bounds of the 95% uncertainty interval across samples

# Get predictions by commune
aggregated_predictions <- model_runner$aggregated_predictions
commune_summary <- aggregated_predictions$commune$summary
summary_sf <- merge(
  x = communes,
  y = commune_summary,
  by = c('commune_code', 'commune', 'department_code', 'department')
)
# join?
summary_sf_lj <- dplyr::left_join(communes, commune_summary)

# Plot aggregated estimates by commune
ggplot2::ggplot() +
  ggplot2::geom_sf(data = summary_sf_lj, ggplot2::aes(fill = mean), color = 'black') +
  ggplot2::scale_fill_gradientn(
    colors = terra::map.pal('viridis'),
    breaks = seq(0.15, 0.45, by = .05),
    labels = scales::percent
  ) +
  ggplot2::labs(
    title = 'MBG mean estimates by commune',
    fill = "Estimated\nstunting\nrate"
  ) +
  ggplot2::theme_minimal()


# look at width of uncertainty:
# Plot aggregated uncertainty interval widths by commune
summary_sf$ui <- summary_sf$upper - summary_sf$lower
ggplot2::ggplot() +
  ggplot2::geom_sf(data = summary_sf, ggplot2::aes(fill = ui), color = 'black') +
  ggplot2::scale_fill_gradientn(
    colors = sf::sf.colors(n = 100),
    labels = scales::percent
  ) +
  ggplot2::labs(
    title = 'MBG 95% uncertainty interval width by commune',
    fill = 'Uncertainty\ninterval\nwidth'
  ) +
  ggplot2::theme_minimal()


# Statistics --------------------------------------------------------------

# Load input data, covariates, and department boundaries
outcomes <- data.table::fread(
  system.file('extdata/child_stunting.csv', package = 'mbg')
)
covariates <- list(
  access = terra::rast(system.file('extdata/access.tif', package = 'mbg')),
  evi = terra::rast(system.file('extdata/evi.tif', package = 'mbg')),
  temperature = terra::rast(system.file('extdata/temperature.tif', package = 'mbg'))
)
covariates$intercept <- covariates[[1]] * 0 + 1
departments <- sf::st_read(
  system.file('extdata/Benin_departments.gpkg', package = 'mbg'),
  quiet = TRUE
)

# Create ID raster
id_raster <- mbg::build_id_raster(
  polygons = departments,
  template_raster = covariates[[1]]
)


## Standard and Stacked ----------------------------------------------------

# Standard model (in-sample)
standard_model_is <- mbg::MbgModelRunner$new(
  input_data = outcomes,
  id_raster = id_raster,
  covariate_rasters = covariates,
  verbose = FALSE
)
standard_model_is$run_mbg_pipeline()

# Stacked generalization model (in-sample)

# Same cross-validation settings
cross_validation_settings <- list(method = 'repeatedcv', number = 5, repeats = 5)

# setup submodels to use:
submodel_settings <- list(enet = NULL, gbm = list(verbose = FALSE), treebag = NULL)

# create stacked model and use integer grouping value
stacking_model_is <- mbg::MbgModelRunner$new(
  input_data = outcomes,
  id_raster = id_raster,
  covariate_rasters = covariates,
  use_stacking = TRUE,
  stacking_cv_settings = cross_validation_settings,
  stacking_model_settings = submodel_settings,
  stacking_prediction_range = c(0, 1),
  stacking_use_admin_bounds = TRUE,
  admin_bounds = departments,
  admin_bounds_id = 'department_code',
  verbose = FALSE
)
stacking_model_is$run_mbg_pipeline()

## Calculate stats -------------------------

# calculate in-sample LPD, WAIC, and RMSE for each model:
standard_model_metrics <- standard_model_is$get_predictive_validity()
standard_model_metrics$model_type <- "Standard"
stacking_model_metrics <- stacking_model_is$get_predictive_validity()
stacking_model_metrics$model_type <- "Stacked ensemble"

metrics_in_sample <- rbind(
  standard_model_metrics,
  stacking_model_metrics
)
metrics_in_sample
# closer to zero is better

# calculate in sample RMSE for each component model from the stacked
outcomes[, data_rate := indicator / samplesize]
ml_submodels <- stacking_model_is$model_covariates

submodel_rmse <- data.table::data.table(
  rmse_is = c(
    mbg::rmse_raster_to_point(
      estimates = ml_submodels$enet,
      validation_data = outcomes,
      outcome_field = 'data_rate'
    ),
    mbg::rmse_raster_to_point(
      estimates = ml_submodels$gbm,
      validation_data = outcomes,
      outcome_field = 'data_rate'
    ),
    mbg::rmse_raster_to_point(
      estimates = ml_submodels$treebag,
      validation_data = outcomes,
      outcome_field = 'data_rate'
    )
  ),
  model_type = c('Elastic net', 'Gradient boosted machines', 'Bagged regression trees')
)
submodel_rmse
# BRTs best here

# Assess better approximate model performance with unobserved data, we will perform 10-fold validation using the input dataset. This requires running both the standard and stacking models 10 times on different subsets of the observed point data. For each of the 10 sub-models, we generate predictive validity metrics based on the held-out portion of the data.

# Note that WAIC is an in-sample predictive validity metric: because in_sample = FALSE, WAIC is not generated during these runs.

# Assign a new `holdout_id` field in the data, which are shuffled integers from 1 to 10
n_holdouts <- 10
outcomes$holdout_id <- seq_len(n_holdouts) |>
  rep(length.out = nrow(outcomes)) |>
  sample()

# For each of the 10 holdouts, get both models' *out-of-sample* predictive validity
metrics_by_holdout <- lapply(seq_len(n_holdouts), function(holdout){
  # Split into (observed) training data and (unobserved) testing data
  train <- outcomes[holdout_id != holdout,]
  test <- outcomes[holdout_id == holdout,]
  # Run both models
  standard_model_oos <- mbg::MbgModelRunner$new(
    input_data = train,
    id_raster = id_raster,
    covariate_rasters = covariates,
    verbose = FALSE
  )
  standard_model_oos$run_mbg_pipeline()
  stacking_model_oos <- mbg::MbgModelRunner$new(
    input_data = train,
    id_raster = id_raster,
    covariate_rasters = covariates,
    use_stacking = TRUE,
    stacking_cv_settings = cross_validation_settings,
    stacking_model_settings = submodel_settings,
    stacking_prediction_range = c(0, 1),
    stacking_use_admin_bounds = TRUE,
    admin_bounds = departments,
    admin_bounds_id = 'department_code',
    verbose = FALSE
  )
  stacking_model_oos$run_mbg_pipeline()

  # Compare to the test data
  standard_metrics <- standard_model_oos$get_predictive_validity(
    in_sample = FALSE,
    validation_data = test
  )[, model_type := 'Standard']
  stacking_metrics <- stacking_model_oos$get_predictive_validity(
    in_sample = FALSE,
    validation_data = test
  )[, model_type := 'Stacked ensemble']
  this_holdout_metrics <- rbind(standard_metrics, stacking_metrics)
  this_holdout_metrics$holdout_id <- holdout

  # Return the combined metrics for this holdout
  return(this_holdout_metrics)
}) |>
  data.table::rbindlist()

# get an overall comparison of out-of-sample predictive validity, we combine these metrics across holdouts:
out_of_sample_overall <- metrics_by_holdout[
  , .(rmse_oos = mean(rmse_oos), lpd_oos = sum(lpd_oos)),
  by = model_type
]
out_of_sample_overall
