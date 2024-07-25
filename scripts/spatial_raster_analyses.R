
library(terra)
library(glue)

# https://github.com/Nowosad/comparing-spatial-patterns-2024

dat <- "/Users/rapeek/Downloads/comparing-spatial-patterns-2024-main/data"

ndvi2018_tartu = rast(glue("{dat}/ndvi2018_tartu.tif"))
ndvi2023_tartu = rast(glue("{dat}/ndvi2023_tartu.tif"))
ndvi2023_poznan = rast(glue("{dat}/ndvi2023_poznan.tif"))
plot(ndvi2018_tartu, main = "Tartu (2018)")
plot(ndvi2023_tartu, main = "Tartu (2023)")
plot(ndvi2023_poznan, main = "Poznań (2023)")


# difference between rasters
ndvi_diff = ndvi2023_tartu - ndvi2018_tartu
plot(ndvi_diff)

# correlation between focal regions
ndvi_cor = focalPairs(c(ndvi2023_tartu, ndvi2018_tartu),
                      w = 5, fun = "pearson", na.rm = TRUE)
plot(ndvi_cor)

# difference between focal measure
# 'sa': average surface roughness
library(geodiv)
window = matrix(1, nrow = 5, ncol = 5)
ndvi2018_tartu_sa_mw = focal_metrics(ndvi2018_tartu,
                                     window = window,
                                     metric = "sa",
                                     progress = FALSE)

ndvi2023_tartu_sa_mw = focal_metrics(ndvi2023_tartu,
                                     window = window,
                                     metric = "sa",
                                     progress = FALSE)

ndvi_diff_sa_mw = ndvi2023_tartu_sa_mw$sa - ndvi2018_tartu_sa_mw$sa
plot(ndvi_diff_sa_mw)

# The difference between a focal measure of two rasters
# library(GLCMTextures)
# ndvi2018_tartu_q = quantize_raster(ndvi2018_tartu,
#                                    n_levels = 16,
#                                    method = "equal prob")
# ndvi2023_tartu_q = quantize_raster(ndvi2023_tartu,
#                                    n_levels = 16,
#                                    method = "equal prob")
#
# ndvi2018_tartu_textures = glcm_textures(ndvi2018_tartu_q,
#                                         w = c(5, 5),
#                                         na.rm = TRUE,
#                                         metrics = "glcm_homogeneity",
#                                         n_levels = 16, quantization = "none")
#
# ndvi2023_tartu_textures = glcm_textures(ndvi2023_tartu_q, w = c(5, 5),
#                                         na.rm = TRUE,
#                                         metrics = "glcm_homogeneity",
#                                         n_levels = 16, quantization = "none")
#
# ndvi2023_tartu_textures_diff = ndvi2023_tartu_textures - ndvi2018_tartu_textures
# plot(ndvi2023_tartu_textures_diff)

# spatial autocorrelation of the differences
ndvi_diff = ndvi2023_tartu - ndvi2018_tartu
ndvi_diff_autocor = autocor(ndvi_diff, method = "moran", global = FALSE)
plot(ndvi_diff_autocor)


# structural similarity SSIM

library(SSIMmap)
ndvi_ssim = ssim_raster(ndvi2018_tartu, ndvi2023_tartu, global = FALSE, w = 5)
plot(ndvi_ssim)

# Rao's quadratic entropy

library(rasterdiv)
ndvi2018_tartu_int = ndvi2018_tartu * 100
ndvi2023_tartu_int = ndvi2023_tartu * 100
ndvi2018_tartu_rao = paRao(ndvi2018_tartu_int, window = 5, progBar = FALSE)
ndvi2023_tartu_rao = paRao(ndvi2023_tartu_int, window = 5, progBar = FALSE)
ndvi_rao_diff = ndvi2023_tartu_rao[[1]][[1]] - ndvi2018_tartu_rao[[1]][[1]]
plot(ndvi_rao_diff)


# single value RMSE

library(yardstick)
ndvi_rmse = rmse_vec(values(ndvi2023_tartu)[,1], values(ndvi2018_tartu)[,1])
ndvi_rmse

# diff btwn raster vals
library(diffeR)
(ndvi_mad = MAD(ndvi2023_tartu, ndvi2018_tartu))

# disimilarity between distributions of two rasters values
library(philentropy)
softmax = function(x) {
  exp_x = exp(x - max(x))
  return(exp_x / sum(exp_x))
}

ndvi2023_tartu_vals = na.omit(values(ndvi2023_tartu)[,1])
ndvi2023_poznan_vals = na.omit(values(ndvi2023_poznan)[,1])

ndvi2023_tartu_vals_prob = softmax(ndvi2023_tartu_vals)
ndvi2023_poznan_vals_prob = softmax(ndvi2023_poznan_vals)

ndvi2023_tartu_vals_prob_interp = approx(seq_along(ndvi2023_tartu_vals_prob),
                                         ndvi2023_tartu_vals_prob,
                                         xout = seq_along(ndvi2023_poznan_vals_prob))$y

ndvi_mat = rbind(ndvi2023_poznan_vals_prob, ndvi2023_tartu_vals_prob_interp)
philentropy::distance(ndvi_mat, method = "kullback-leibler")

# average structural similarity index
library(SSIMmap)
ssim_raster(ndvi2018_tartu, ndvi2023_tartu, global = TRUE)


# distribution of differences between rasters
ndvi_diff = ndvi2023_tartu - ndvi2018_tartu
hist(ndvi_diff)


# Statistics of the differences between rasters’ values calculated at many scales
library(waywiser)
cell_sizes = c(200, 400, 600)
ndvi_multi_scale = ww_multi_scale(truth = ndvi2018_tartu, estimate = ndvi2023_tartu,
                                  metrics = list(yardstick::rmse),
                                  cellsize = cell_sizes,
                                  progress = FALSE)
ndvi_multi_scale
