

# test sampling
library(sf)
library(tigris)
library(tidyverse)
library(units)


# GET A SHAPE -------------------------------------------------------------

ca <- states() |> filter(STUSPS == "CA")
cnty <- counties(state = "CA")
sierra <- filter(cnty, NAME=="Sierra") |> st_transform(3310)


# MAKE GRID ---------------------------------------------------------------

# Create a grid of points
grid <- st_make_grid(sierra, cellsize = 500, what = "polygons", square = F) |>
  st_intersection(st_union(sierra))
grid_pts <- st_make_grid(sierra, cellsize = 500, what = "centers", square = F) |> st_as_sf() |> st_intersection(st_union(sierra))

# sample random
pts_rando <- st_sample(sierra, type="random", size=2000) |> st_as_sf()

plot(sierra$geometry)
plot(grid_pts$x, col="green4", pch=16, cex=0.3, add=T)
plot(pts_rando$x, col="purple", pch=16, cex=0.5, add=T)


# MAKE ATTRIBS ---------------------------------------------------------

# Create an attribute column (replace 'category' with your actual column name)
pts_rando$category <- sample(c("A", "B", "C"), size = nrow(pts_rando), replace = T)
table(pts_rando$category)
pts_rando <- rowid_to_column(pts_rando, var = "id")

# SELECT POINTS WITHIN 250 to 700 METERS OF ONE ANOTHER -------------------

filter_points_within_distance <- function(sf_data, min_distance = 250, max_distance = 700) {
  # Create a matrix of distances between points
  distances_matrix <- st_distance(sf_data)

  # Check if there is at least one point within the specified distance range
  has_neighbor <- apply(distances_matrix, 1, function(row) any(row > min_distance & row < max_distance))

  # Filter points based on the condition
  selected_points <- sf_data[has_neighbor, ]

  return(selected_points)
}

# Filter points with at least one other point within 250 to 700 meters
result <- filter_points_within_distance(pts_rando, min_distance = 250, max_distance = 700)


## MAPVIEW RESULT ----------------------------------------------------------

library(mapview)
library(leaflet)
m1 <- mapview(result, zcol="category", alpha.regions=0.7) +
  #mapview(grid_pts, cex=2.4, col.regions="orange") +
  mapview(pts_rando, cex=2.4, col.regions="orange")
m1@map |> addMeasure(primaryLengthUnit = "meters")

# Plot the result
plot(sierra$geometry, col = "black", main = "Selected Points with Different Adjacent Categories")
plot(grid_pts$x, col = "white", pch = 16, add=T, cex=0.7)
plot(result$x, col="purple", cex=0.6, pch=16, add=T)

# SELECT CLUSTER ----------------------------------------------------------

library(sampling)

# min_distance <- set_units(min_distance, "m")
# max_distance <- set_units(max_distance, "m")

## OPTION 1 ---------------

# select any cluster that maximizes diversity and falls within 700 m of one another
select_all_diverse_clusters <- function(sf_data, category_column = "category", max_distance = 700, cluster_size = 4) {

  max_distance <- set_units(max_distance, "m")

  # Calculate pairwise distances between points
  distances <- st_distance(sf_data)

  # Initialize variables to store clusters and their diversity
  clusters <- list()
  clusters_diversity <- numeric()

  # Iterate through each point
  for (i in 1:nrow(sf_data)) {
    # Check if the point is within the specified distance range of existing clusters
    within_distance <- sapply(clusters, function(cluster) all(distances[i, cluster] <= max_distance))

    # Check if the point satisfies the condition
    if (any(within_distance)) {
      # Add the point to the first cluster that satisfies the condition
      cluster_index <- which(within_distance)[1]
      clusters[[cluster_index]] <- c(clusters[[cluster_index]], i)
      clusters_diversity[cluster_index] <- length(unique(sf_data[[category_column]][clusters[[cluster_index]]]))
    } else {
      # If the point does not fit in any existing cluster, create a new cluster
      clusters[[length(clusters) + 1]] <- i
      clusters_diversity <- c(clusters_diversity, 1)  # Diversity starts at 1 for the new cluster
    }
  }

  # Find clusters with equal or maximum diversity in categories
  max_diversity <- max(clusters_diversity)
  selected_clusters <- clusters[clusters_diversity == max_diversity]

  # Filter clusters with at least cluster_size points
  selected_clusters <- selected_clusters[sapply(selected_clusters, length) >= cluster_size]

  # Concatenate points from selected clusters
  selected_points <- sf_data[unlist(selected_clusters), ]

  return(selected_points)
}

# run it
selected_points_all <- select_all_diverse_clusters(result, category_column = "category")
beepr::beep()

# plot it
plot(sierra$geometry, col = "black", main = "Selected Points with Different Adjacent Categories")
plot(pts_rando$x, col = "white", pch = 16, add=T, cex=0.7)
plot(selected_points_all, col="purple", lwd=2, bg="gray", cex=1, pch=21, add=T)

m2 <- mapview(selected_points_all, zcol="category", alpha.regions=0.7) +
  mapview(pts_rando, cex=2.4, col.regions="orange")
m2@map |> addMeasure(primaryLengthUnit = "meters")




