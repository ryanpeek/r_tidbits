

# test sampling
library(sf)
library(tigris)
library(tidyverse)
library(units)

# get ca and counties
ca <- states() |> filter(STUSPS == "CA")
cnty <- counties(state = "CA")
sierra <- filter(cnty, NAME=="Sierra") |> st_transform(3310)
# Create a grid of points
grid <- st_make_grid(sierra, cellsize = 500, what = "polygons", square = F) |>
  st_intersection(st_union(sierra))
grid_pts <- st_make_grid(sierra, cellsize = 500, what = "centers", square = F) |> st_as_sf() |> st_intersection(st_union(sierra))

# sample random
grid_pts <- st_sample(sierra, type="random", size=2000) |> st_as_sf()

plot(sierra$geometry)
#plot(grid, add=T)
plot(grid_pts$x, col="green4", pch=16, cex=0.5, add=T)

# Create an attribute column (replace 'category' with your actual column name)
grid_pts$category <- sample(c("A", "B", "C"), size = nrow(grid_pts), replace = T)
table(grid_pts$category)
grid_pts <- rowid_to_column(grid_pts, var = "id")

#dist_buff <- set_units(500, "m")

# FUNCTION TO ONLY SELECT POINTS WITHIN 250 to 700 METERS OF ONE ANOTHER
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
result <- filter_points_within_distance(grid_pts, min_distance = 250, max_distance = 700)

library(mapview)
library(leaflet)
m1 <- mapview(result, zcol="category", alpha.regions=0.7) + mapview(grid_pts, cex=2.4, col.regions="orange")
m1@map |> addMeasure(primaryLengthUnit = "meters")

# Plot the result
plot(sierra$geometry, col = "black", main = "Selected Points with Different Adjacent Categories")
plot(grid_pts$x, col = "white", pch = 16, add=T, cex=0.7)
plot(result$x, col="purple", cex=0.6, pch=16, add=T)


# now with this result, can we select by type?
library(sampling)

# min_distance <- set_units(min_distance, "m")
# max_distance <- set_units(max_distance, "m")

# OPTION 1: --------------
select_diverse_cluster <- function(sf_data, category_column = "category", min_distance = 250, max_distance = 700) {

  min_distance <- set_units(min_distance, "m")
  max_distance <- set_units(max_distance, "m")

  # Initialize variables to store clusters and their diversity
  clusters <- list()
  clusters_diversity <- numeric()
  distances <- st_distance(sf_data) # pairwise

  # Iterate through each point
  for (i in 1:nrow(sf_data)) {
    within_distance <- sapply(clusters, function(cluster) all(distances[i, cluster] <= max_distance))

    # Check if the point is greater than the minimum distance from all clusters
    greater_than_min_distance <- sapply(clusters, function(cluster) all(distances[i, cluster] > min_distance))

    # Check if the point satisfies both conditions
    if (any(within_distance) && all(greater_than_min_distance)) {
      # Add the point to the first cluster that satisfies the conditions
      cluster_index <- which(within_distance)[1]
      clusters[[cluster_index]] <- c(clusters[[cluster_index]], i)
      clusters_diversity[cluster_index] <- length(unique(sf_data$category[clusters[[cluster_index]]]))
    } else {
      # If the point does not fit in any existing cluster, create a new cluster
      clusters[[length(clusters) + 1]] <- i
      clusters_diversity <- c(clusters_diversity, 1)  # Diversity starts at 1 for the new cluster
    }
  }

  # Find the cluster with the maximum diversity in categories
  if (length(clusters) > 0) {
    max_diversity_cluster <- clusters[[which.max(clusters_diversity)]]
    selected_cluster <- sf_data[max_diversity_cluster, ]
    return(selected_cluster)
  } else {
    warning("No clusters found.")
    return(NULL)
  }
}

# run it
selected_cluster <- select_diverse_cluster(result, category_column = "category")

plot(sierra$geometry, col = "black", main = "Selected Points with Different Adjacent Categories")
plot(grid_pts$x, col = "white", pch = 16, add=T, cex=0.7)
plot(selected_cluster, col="purple", cex=2, pch=16, add=T)

# OPTION 2 ---------------
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

selected_points_all <- select_all_diverse_clusters(result, category_column = "category")

plot(sierra$geometry, col = "black", main = "Selected Points with Different Adjacent Categories")
plot(grid_pts$x, col = "white", pch = 16, add=T, cex=0.7)
plot(selected_points_all, col="purple", cex=2, pch=16, add=T)
