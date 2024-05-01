# Code is presented in chronological order based on time it was prompted in ChatGPT...each attempt is separated into different section heads

# Results:
  ## CHAT GPT ATTEMPT #1: Fail - incorrect point placement
  ## CHAT GPT ATTEMPT #1.1: Fail - incorrect packages/functions
  ## CHAT GPT ATTEMPT #2: Fail - incorrect object usage
  ## CHAT GPT ATTEMPT #2.1: Pass - simple map with GPS coordinates
  ## CHAT GPT ATTEMPT #3: Fail - data type mismatch
  ## CHAT GPT ATTEMPT #3.1: Fail - raster way too large
  ## CHAT GPT ATTEMPT #4: Fail - data type mismatch, too large
  ## CHAT GPT ATTEMPT #4.1: ~Pass - created raster, but not meaningful
  ## CHAT GPT ATTEMPT #5: Fail - data mismatch

## CHAT GPT ATTEMPT #1 ##--------------------------------------------------------------
  
# Load required libraries
  library(ggplot2)
  library(maps)
  
  # Generate fictional GPS coordinates for 100 coyotes within Texas
  set.seed(123) # for reproducibility
  coyotes <- data.frame(
    longitude = runif(100, min = -106.65, max = -93.51), # Texas longitudinal boundaries
    latitude = runif(100, min = 25.84, max = 36.5) # Texas latitudinal boundaries
  )
  
  # Create a basemap of Texas
  texas_map <- map_data("state", region = "texas")
  
  # Plot GPS coordinates on the basemap
  ggplot() +
    geom_polygon(data = texas_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = coyotes, aes(x = longitude, y = latitude), color = "red", size = 2) +
    labs(title = "GPS Coordinates of Coyotes in Texas") +
    theme_minimal()

## CHAT GPT ATTEMPT #1.1 - revision##---------------------------------------------------
  
  # Load required libraries
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  
  # Get the shape of Texas from the rnaturalearth package
  texas <- ne_states(country = "united states", state = "texas", returnclass = "sf")
  
  #Attempt 2#
  texas <- st_read(system.file("shape/usa/texas.shp", package="rnaturalearth"))
  
  # Create a function to generate random points within a polygon
  generate_points_within_polygon <- function(poly, n) {
    bbox <- st_bbox(poly)
    points <- st_sample(poly, n)
    points_df <- as.data.frame(st_coordinates(points))
    colnames(points_df) <- c("longitude", "latitude")
    return(points_df)
  }
  
  # Generate 100 random points within the Texas polygon
  set.seed(123) # for reproducibility
  coyotes <- generate_points_within_polygon(texas, 100)
  
  # Plot GPS coordinates on the basemap
  ggplot() +
    geom_sf(data = texas, fill = "white", color = "black") +
    geom_point(data = coyotes, aes(x = longitude, y = latitude), color = "red", size = 2) +
    labs(title = "GPS Coordinates of Coyotes in Texas") +
    theme_minimal()


## CHAT GPT ATTEMPT #2 - packages##---------------------------------------------------

  # Load required libraries
  library(sf)
  library(raster)
  library(tmap)
  
  # Load shapefile for Texas
  texas_shapefile <- st_read("State.shp")
  
  # Generate random points within Texas
  generate_points_within_polygon <- function(poly, n) {
    bbox <- st_bbox(poly)
    points <- st_sample(poly, n)
    points_df <- as.data.frame(st_coordinates(points))
    colnames(points_df) <- c("longitude", "latitude")
    return(points_df)
  }
  
  set.seed(123) # for reproducibility
  coyotes <- generate_points_within_polygon(texas_shapefile, 100)
  
  # Plot GPS coordinates on a basemap of Texas
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_shape(coyotes) +
    tm_dots(col = "red") +
    tm_layout(title = "GPS Coordinates of Coyotes in Texas") +
    tm_legend(outside = TRUE)


## CHAT GPT ATTEMPT #2.1 - packages---------------------------------------------------
  
  # Load required libraries
  library(sf)
  library(raster)
  library(tmap)
  
  # Load shapefile for Texas
  texas_shapefile <- st_read("State.shp")
  
  # Generate random points within Texas
  generate_points_within_polygon <- function(poly, n) {
    bbox <- st_bbox(poly)
    points <- st_sample(poly, n)
    points_df <- as.data.frame(st_coordinates(points))
    colnames(points_df) <- c("longitude", "latitude")
    return(st_as_sf(points_df, coords = c("longitude", "latitude"), crs = st_crs(poly)))
  }
  
  set.seed(123) # for reproducibility
  coyotes <- generate_points_within_polygon(texas_shapefile, 100)
  
  # Plot GPS coordinates on a basemap of Texas
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_shape(coyotes) +
    tm_dots(col = "red") +
    tm_layout(title = "GPS Coordinates of Coyotes in Texas") +
    tm_legend(outside = TRUE)

## CHAT GPT ATTEMPT #3 - improve map##---------------------------------------------------

  # Load required libraries
  library(sf)
  library(raster)
  library(tmap)

  # Load shapefile for Texas
  texas_shapefile <- st_read("State.shp")
  
  # Create an empty raster layer covering Texas
  raster_template <- raster(extent(texas_shapefile), resolution = 0.1)
  
  # Generate random land cover types
  set.seed(123) # for reproducibility
  land_cover <- sample(c("forest", "water", "urban", "field", "desert"), ncell(raster_template), replace = TRUE)
  
  # Assign land cover classes to the raster
  raster_land_cover <- setValues(raster_template, land_cover)
  
  # Plot the raster
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_raster(raster_land_cover, palette = "Set1", title = "Land Cover") +
    tm_layout(title = "Land Cover in Texas")
  
  # Overlay coyote points on the land cover raster
  coyotes_with_land_cover <- st_join(coyotes, raster_land_cover)
  
  # Plot GPS coordinates on a basemap of Texas with land cover
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_raster(raster_land_cover, palette = "Set1", title = "Land Cover") +
    tm_shape(coyotes_with_land_cover) +
    tm_dots(col = "red") +
    tm_layout(title = "GPS Coordinates of Coyotes with Land Cover in Texas") +
    tm_legend(outside = TRUE)

## CHAT GPT ATTEMPT #3.1 - revisions##---------------------------------------------------

  #Attempt 1
  # Load required libraries
  library(sf)
  library(raster)
  library(tmap)

  # Load shapefile for Texas
  texas_shapefile <- st_read("State.shp")
  
  # Limit the extent of Texas to reduce memory usage
  texas_bbox <- st_bbox(texas_shapefile)
  extent_buffer <- 0.5 # degree buffer
  texas_bbox_buffered <- c(texas_bbox[1] - extent_buffer, texas_bbox[2] - extent_buffer,
                           texas_bbox[3] + extent_buffer, texas_bbox[4] + extent_buffer)
  texas_extent <- extent(texas_bbox_buffered)
  
  # Create an empty raster layer covering Texas with reduced resolution
  raster_template <- raster(texas_extent, resolution = 0.2)
  
  # Generate random land cover types
  set.seed(123) # for reproducibility
  land_cover <- sample(c("forest", "water", "urban", "field", "desert"), ncell(raster_template), replace = TRUE)
  
  # Assign land cover classes to the raster
  raster_land_cover <- setValues(raster_template, land_cover)
  
  # Plot the raster
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_raster(raster_land_cover, palette = "Set1", title = "Land Cover") +
    tm_layout(title = "Land Cover in Texas")
  
  # Overlay coyote points on the land cover raster
  coyotes_with_land_cover <- st_join(coyotes, raster_land_cover)
  
  # Plot GPS coordinates on a basemap of Texas with land cover
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_raster(raster_land_cover, palette = "Set1", title = "Land Cover") +
    tm_shape(coyotes_with_land_cover) +
    tm_dots(col = "red") +
    tm_layout(title = "GPS Coordinates of Coyotes with Land Cover in Texas") +
    tm_legend(outside = TRUE)

  #Attempt 2
  # Load required libraries
  library(sf)
  library(raster)
  library(tmap)

  # Load shapefile for Texas
  texas_shapefile <- st_read("State.shp")
  
  # Limit the extent of Texas to reduce memory usage
  texas_bbox <- st_bbox(texas_shapefile)
  extent_buffer <- 0.5 # degree buffer
  texas_bbox_buffered <- c(texas_bbox[1] - extent_buffer, texas_bbox[2] - extent_buffer,
                           texas_bbox[3] + extent_buffer, texas_bbox[4] + extent_buffer)
  texas_extent <- extent(texas_bbox_buffered)
  
  # Create an empty raster layer covering Texas with reduced resolution
  raster_template <- raster(texas_extent, res = 0.5)
  
  # Generate random land cover types
  set.seed(123) # for reproducibility
  land_cover <- sample(c("forest", "water", "urban", "field", "desert"), ncell(raster_template), replace = TRUE)
  
  # Assign land cover classes to the raster
  raster_land_cover <- setValues(raster_template, land_cover)
  
  # Plot the raster
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_raster(raster_land_cover, palette = "Set1", title = "Land Cover") +
    tm_layout(title = "Land Cover in Texas")
  
  # Overlay coyote points on the land cover raster
  coyotes_with_land_cover <- st_join(coyotes, raster_land_cover)
  
  # Plot GPS coordinates on a basemap of Texas with land cover
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_raster(raster_land_cover, palette = "Set1", title = "Land Cover") +
    tm_shape(coyotes_with_land_cover) +
    tm_dots(col = "red") +
    tm_layout(title = "GPS Coordinates of Coyotes with Land Cover in Texas") +
    tm_legend(outside = TRUE)

  #Attempt 3
  # Load required libraries
  library(sf)
  library(raster)
  library(tmap)

  # Load shapefile for Texas
  texas_shapefile <- st_read("State.shp")
  
  # Limit the extent of Texas to reduce memory usage
  texas_bbox <- st_bbox(texas_shapefile)
  extent_buffer <- 0.5 # degree buffer
  texas_bbox_buffered <- c(texas_bbox[1] - extent_buffer, texas_bbox[2] - extent_buffer,
                           texas_bbox[3] + extent_buffer, texas_bbox[4] + extent_buffer)
  texas_extent <- extent(texas_bbox_buffered)
  
  # Create a smaller raster layer covering Texas with reduced resolution
  raster_template <- raster(texas_extent, res = 0.5)
  
  # Create a grid of cells covering Texas
  grid_cells <- as(raster_template, "SpatialPolygonsDataFrame")
  
  # Generate random land cover types for each grid cell
  set.seed(123) # for reproducibility
  land_cover <- sample(c("forest", "water", "urban", "field", "desert"), nrow(grid_cells), replace = TRUE)
  
  # Assign land cover classes to the grid cells
  grid_cells$land_cover <- land_cover
  
  # Convert the grid cells to a raster
  raster_land_cover <- rasterize(grid_cells, raster_template, field = "land_cover")
  
  # Plot the raster
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_raster(raster_land_cover, palette = "Set1", title = "Land Cover") +
    tm_layout(title = "Land Cover in Texas")
  
  # Overlay coyote points on the land cover raster
  coyotes_with_land_cover <- st_join(coyotes, raster_land_cover)
  
  # Plot GPS coordinates on a basemap of Texas with land cover
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_raster(raster_land_cover, palette = "Set1", title = "Land Cover") +
    tm_shape(coyotes_with_land_cover) +
    tm_dots(col = "red") +
    tm_layout(title = "GPS Coordinates of Coyotes with Land Cover in Texas") +
    tm_legend(outside = TRUE)

  #Attempt 4

  # Load required libraries
  library(sf)
  library(raster)
  library(tmap)
  
  # Load shapefile for Texas
  texas_shapefile <- st_read("State.shp")
  
  # Limit the extent of Texas to reduce memory usage
  texas_bbox <- st_bbox(texas_shapefile)
  extent_buffer <- 0.2 # degree buffer
  texas_bbox_buffered <- c(texas_bbox[1] - extent_buffer, texas_bbox[2] - extent_buffer,
                           texas_bbox[3] + extent_buffer, texas_bbox[4] + extent_buffer)
  texas_extent <- extent(texas_bbox_buffered)
  
  # Create a smaller raster layer covering Texas with further reduced resolution
  raster_template <- raster(texas_extent, res = 0.5)
  
  # Convert Texas shapefile to raster
  raster_texas <- rasterize(texas_shapefile, raster_template)
  
  # Generate random land cover types
  set.seed(123) # for reproducibility
  land_cover <- sample(c("forest", "water", "urban", "field", "desert"), ncell(raster_texas), replace = TRUE)
  
  # Assign land cover classes to the raster
  raster_land_cover <- setValues(raster_texas, land_cover)
  
  # Plot the raster
  tm_shape(raster_land_cover) +
    tm_raster(palette = "Set1", title = "Land Cover") +
    tm_layout(title = "Land Cover in Texas")
  
  # Overlay coyote points on the land cover raster
  coyotes_with_land_cover <- st_join(coyotes, raster_land_cover)
  
  # Plot GPS coordinates on a basemap of Texas with land cover
  tm_shape(raster_land_cover) +
    tm_raster(palette = "Set1", title = "Land Cover") +
    tm_shape(coyotes_with_land_cover) +
    tm_dots(col = "red") +
    tm_layout(title = "GPS Coordinates of Coyotes with Land Cover in Texas") +
    tm_legend(outside = TRUE)


## CHAT GPT ATTEMPT #4 - break-up##---------------------------------------------------
  
  #Attempt 1#
  # Load required libraries
  library(tmap)
  library(sf)
  library(raster)
  
  # Load shapefile for Texas
  texas_shapefile <- st_read("State.shp")
  
  # Set the extent of the raster to match the outline of Texas
  texas_extent <- st_bbox(texas_shapefile)
  
  # Calculate the number of rows and columns for the raster with a resolution of 10,000 km² per cell
  cell_area <- 10000 # km²
  n_rows <- ceiling((texas_extent[4] - texas_extent[2]) / sqrt(cell_area))
  n_cols <- ceiling((texas_extent[3] - texas_extent[1]) / sqrt(cell_area))
  
  # Create the raster layer
  raster_land_cover <- raster(
    extent(texas_extent),
    nrows = n_rows,
    ncols = n_cols,
    crs = st_crs(texas_shapefile),
    resolution = sqrt(cell_area)
  )
  
  # Assign random land cover classes to each cell
  set.seed(123) # for reproducibility
  land_cover_classes <- c("forest", "water", "urban", "field", "desert")
  land_cover <- sample(land_cover_classes, ncell(raster_land_cover), replace = TRUE)
  raster_land_cover[] <- land_cover
  
  # Plot the raster
  plot(raster_land_cover)
  
  #Attempt 2#
  library(tmap)
  library(sf)
  library(raster)
  
  # Load shapefile for Texas
  texas_shapefile <- st_read("State.shp")
  
  # Set the extent of the raster to match the outline of Texas
  texas_extent <- st_bbox(texas_shapefile)
  
  # Randomly select a 10,000 km² square within Texas
  texas_bbox <- st_bbox(texas_shapefile)
  square_extent <- sampleRandom(texas_shapefile, size = 1, ext = texas_bbox, sp = TRUE)
  
  xmin <- runif(1, texas_bbox["xmin"], texas_bbox["xmax"] - 100)
  xmax <- xmin + 100
  ymin <- runif(1, texas_bbox["ymin"], texas_bbox["ymax"] - 100)
  ymax <- ymin + 100
  
  # Create an extent object for the selected square
  square_extent <- extent(xmin, xmax, ymin, ymax)
  
  # Set the extent of the raster to match the selected square
  resolution <- 0.2 # Use a smaller resolution to reduce the raster size
  raster_land_cover_small <- raster(
    square_extent,
    res = resolution,
    crs = st_crs(texas_shapefile),
    vals = NA
  )
  
  # Assign random land cover classes to each cell within the selected square
  set.seed(123) # for reproducibility
  land_cover_classes <- c("forest", "water", "urban", "field", "desert")
  land_cover_small <- sample(1:length(land_cover_classes), ncell(raster_land_cover_small), replace = TRUE)
  values(raster_land_cover_small) <- land_cover_small
  
  
  # Plot the raster
  plot(raster_land_cover_small)
  
  # Plot the raster
  plot(raster_land_cover_small, col = terrain.colors(5), legend = FALSE)
  
  # Plot the basemap
  plot(texas_shapefile, add = TRUE)
  
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_layout(frame = FALSE)
  
  texas_extent <- extent(texas_bbox)
  
  #Attempt 3#
  library(tmap)
  library(sf)
  library(raster)
  
  # Load shapefile for Texas
  texas_shapefile <- st_read("State.shp")
  
  # Set the extent of the raster to match the outline of Texas
  texas_extent <- st_bbox(texas_shapefile)
  
  # Set the resolution of the raster to 10,000 km² (arbitrary value)
  resolution <- sqrt(10000) # Square root of the area of Texas in km²
  
  # Create a single-cell raster covering the extent of Texas
  raster_land_cover <- raster(
    texas_extent,
    res = resolution,
    crs = st_crs(texas_shapefile),
    vals = NA
  )
  
  # Assign a random land cover class to the single cell
  set.seed(123) # for reproducibility
  land_cover_classes <- 1:5
  land_cover <- sample(land_cover_classes, 1)
  values(raster_land_cover) <- land_cover
  
  # Plot the raster
  plot(raster_land_cover, col = terrain.colors(5))
  
## CHAT GPT ATTEMPT #4.1 ---------------------------------------------------
  
  # Load required libraries
  library(sf)
  library(raster)
  
  # Load shapefile for Texas
  texas_shapefile <- st_read("State.shp")
  
  # Set seed for reproducibility
  set.seed(123)
  
  # Get the bounding box of Texas
  texas_bbox <- st_bbox(texas_shapefile)
  
  # Randomly select a 10,000 km² square within Texas
  xmin <- runif(1, texas_bbox["xmin"], texas_bbox["xmax"] - 100)
  xmax <- xmin + 100
  ymin <- runif(1, texas_bbox["ymin"], texas_bbox["ymax"] - 100)
  ymax <- ymin + 100
  
  # Create an extent object for the selected square
  square_extent <- extent(xmin, xmax, ymin, ymax)
  
  # Set the extent of the raster to match the selected square
  resolution <- 0.2 # Use a smaller resolution to reduce the raster size
  raster_land_cover_small <- raster(
    square_extent,
    res = resolution,
    crs = st_crs(texas_shapefile),
    vals = NA
  )
  
  # Assign random land cover classes to each cell within the selected square
  set.seed(123) # for reproducibility
  land_cover_classes <- c("forest", "water", "urban", "field", "desert")
  land_cover_small <- sample(1:length(land_cover_classes), ncell(raster_land_cover_small), replace = TRUE)
  values(raster_land_cover_small) <- land_cover_small
  
  # Plot the raster
  plot(raster_land_cover_small)
  
## CHAT GPT ATTEMPT #5##---------------------------------------------------
  
  #Attempt 1#
  # Load required libraries
  library(sf)
  library(raster)
  library(tmap)

  # Load shapefile for Texas
  texas_shapefile <- st_read("State.shp")
  
  # Read in the raster file representing land cover in Texas
  raster_land_cover <- raster("gaplf2011lc_v30_tx.tif")
  
  # Plot the raster
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_raster(raster_land_cover, palette = "Set1", title = "Land Cover") +
    tm_layout(title = "Land Cover in Texas")
  
  # Overlay coyote points on the land cover raster
  coyotes_with_land_cover <- st_join(coyotes, raster_land_cover)
  
  # Plot GPS coordinates on a basemap of Texas with land cover
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_raster(raster_land_cover, palette = "Set1", title = "Land Cover") +
    tm_shape(coyotes_with_land_cover) +
    tm_dots(col = "red") +
    tm_layout(title = "GPS Coordinates of Coyotes with Land Cover in Texas") +
    tm_legend(outside = TRUE)

  #error - minor correction
  
  # Plot the raster
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_raster(raster_land_cover, palette = "Set1", title = "Land Cover") +
    tm_layout(title = "Land Cover in Texas")
  
  # Overlay coyote points on a basemap of Texas with land cover
  tm_shape(coyotes_with_land_cover) +
    tm_dots(col = "red") +
    tm_layout(title = "GPS Coordinates of Coyotes with Land Cover in Texas") +
    tm_legend(outside = TRUE)
  
  #error
  
  #Attempt 2#
  # Load required libraries
  library(sf)
  library(raster)
  library(tmap)

  # Load shapefile for Texas
  texas_shapefile <- st_read(system.file("extdata", "texas.shp", package = "sf"))
  
  # Read in the raster file representing land cover in Texas
  raster_land_cover <- raster("path/to/your/raster/file.tif")
  
  # Create a tmap object with the raster
  tm <- tm_shape(raster_land_cover) +
    tm_raster(palette = "Set1", title = "Land Cover") +
    tm_layout(title = "Land Cover in Texas")
  
  # Overlay coyote points on the raster
  tm <- tm +
    tm_shape(coyotes_with_land_cover) +
    tm_dots(col = "red") +
    tm_layout(title = "GPS Coordinates of Coyotes with Land Cover in Texas") +
    tm_legend(outside = TRUE)
  
  # Plot the tmap object
  tm

  #Attempt 3#
  # Load required libraries
  library(sf)
  library(raster)
  library(tmap)

  # Load shapefile for Texas
  texas_shapefile <- st_read(system.file("extdata", "texas.shp", package = "sf"))
  
  # Read in the raster file representing land cover in Texas
  raster_land_cover <- raster("path/to/your/raster/file.tif")
  
  # Generate random coyote points within Texas
  set.seed(123) # for reproducibility
  coyotes <- st_sample(texas_shapefile, 100, type = "random")
  
  # Extract land cover values for each coyote point
  coyotes$land_cover <- extract(raster_land_cover, coyotes)
  
  # Plot the raster
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_raster(raster_land_cover, palette = "Set1", title = "Land Cover") +
    tm_layout(title = "Land Cover in Texas")
  
  # Plot GPS coordinates on a basemap of Texas with land cover
  tm_shape(coyotes) +
    tm_dots(col = "red") +
    tm_layout(title = "GPS Coordinates of Coyotes with Land Cover in Texas") +
    tm_legend(outside = TRUE)

  #Attempt 4#
  # Load required libraries
  library(sf)
  library(raster)
  library(tmap)

  # Load shapefile for Texas
  texas_shapefile <- st_read(system.file("extdata", "texas.shp", package = "sf"))
  
  # Read in the raster file representing land cover in Texas
  raster_land_cover <- raster("path/to/your/raster/file.tif")
  
  # Generate random coyote points within Texas
  set.seed(123) # for reproducibility
  coyotes <- st_sample(texas_shapefile, 100, type = "random")
  
  # Extract land cover values for each coyote point
  coyotes$land_cover <- extract(raster_land_cover, as(coyotes, "Spatial"))
  
  # Plot the raster
  tm_shape(texas_shapefile) +
    tm_borders() +
    tm_raster(raster_land_cover, palette = "Set1", title = "Land Cover") +
    tm_layout(title = "Land Cover in Texas")
  
  # Plot GPS coordinates on a basemap of Texas with land cover
  tm_shape(coyotes) +
    tm_dots(col = "red", size = 0.5, title = "Land Cover") +
    tm_layout(title = "GPS Coordinates of Coyotes with Land Cover in Texas") +
    tm_legend(outside = TRUE)

#END CODE
