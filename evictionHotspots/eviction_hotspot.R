# Install usethis
install.packages("usethis")

# Create a package named 'evictionHotspots'
usethis::create_package("C:/Users/HP ELITEBOOK 840 G4/Downloads/evictionHotspots")

setwd("C:/Users/HP ELITEBOOK 840 G4/Downloads/evictionHotspots")

Package: evictionHotspots
#Title: Eviction Hotspots Analysis
#Version: 0.1.0
#Authors@R:
 # person(given = "YourName",
        # family = "YourSurname",
        # role = c("aut", "cre"),
         #email = "your.email@example.com")
#Description: Tools for analyzing eviction data and identifying spatial hotspots using Moran's I and LISA.
#License: GPL-3
#Encoding: UTF-8
#RoxygenNote: 7.2.3
#Imports:
    #sf,
    #spdep,
    #tmap,
    #leaflet,
    #shiny,
    #dplyr

#' Calculate eviction hotspots
#'
#' This function calculates global and local Moran's I for eviction data.
#'
#' @param data A spatial dataset (sf object) containing eviction data.
#' @param variable The column name representing eviction counts or rates.
#' @param k Number of nearest neighbors to use for spatial weights.
#' @return A list containing the global Moran's I result and updated data with local Moran's I values.
#' @export
calculate_hotspots <- function(data, variable, k = 4) {
  library(sf)
  library(spdep)
  
  # Extract coordinates
  coords <- st_coordinates(data)
  knn_neighbors <- knn2nb(knearneigh(coords, k = k))
  lw <- nb2listw(knn_neighbors, style = "W")
  
  # Global Moran's I
  global_moran <- moran.test(data[[variable]], lw)
  
  # Local Moran's I
  local_moran <- localmoran(data[[variable]], lw)
  
  # Add LISA to data
  data$LISA <- local_moran[, 1]
  data$hotspot <- ifelse(data$LISA > mean(data$LISA, na.rm = TRUE), "Hotspot", "Not Hotspot")
  
  list(global_moran = global_moran, data = data)
}

#' Plot eviction hotspots
#'
#' This function visualizes eviction hotspots using static or interactive maps.
#'
#' @param data A spatial dataset (sf object) with hotspot classifications.
#' @param hotspot_column The column name representing hotspot classifications.
#' @param interactive Logical; if TRUE, creates an interactive map.
#' @export
plot_hotspots <- function(data, hotspot_column = "hotspot", interactive = FALSE) {
  library(tmap)
  library(leaflet)
  
  if (!interactive) {
    # Static map
    tm_shape(data) +
      tm_polygons(hotspot_column, palette = c("red", "gray"), title = "Hotspots")
  } else {
    # Interactive map
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ifelse(data[[hotspot_column]] == "Hotspot", "red", "gray"),
        color = "black", weight = 1, fillOpacity = 0.7,
        popup = ~paste("Hotspot:", data[[hotspot_column]])
      ) %>%
      addLegend("bottomright", colors = c("red", "gray"),
                labels = c("Hotspot", "Not Hotspot"), title = "Hotspots")
  }
}

install.packages("devtools")
library(devtools)

devtools::document()

# Load the package
devtools::load_all()

# Test the calculate_hotspots function
library(sf)
example_data <- st_as_sf(data.frame(
  id = 1:10,
  evictions = sample(1:100, 10),
  lon = runif(10, -100, -90),
  lat = runif(10, 30, 40)
), coords = c("lon", "lat"), crs = 4326)

result <- calculate_hotspots(example_data, "evictions")
print(result$global_moran)


install.packages("sf")
install.packages("tmap")
packageVersion("sf")


# Test the plot_hotspots function
library(tmap)

# Set interactive view mode
tmap_mode("view")

# Plot using tm_dots for spatial points
tm_shape(result$data) +
  tm_dots(col = "hotspot", palette = c("red", "gray"), size = 0.3, title = "Hotspots") +
  tm_layout(title = "Eviction Hotspots", legend.outside = TRUE)

usethis::use_gpl3_license()


devtools::document()

devtools::check()
devtools::build()


devtools::build()
install.packages("path/to/evictionHotspots.tar.gz", repos = NULL, type = "source")



