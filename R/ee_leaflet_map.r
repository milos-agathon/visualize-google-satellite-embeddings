ee_leaflet_map <- function(
    image,
    vis_params = list(
      min = 0, max = 1,
      palette = c("blue", "red")
    ),
    center,
    zoom = 11,
    satellite_base = TRUE,
    layer_name = "EE layer") {
  message("Creating interactive map for: ", layer_name)

  # Request map tiles from Earth Engine servers
  # getMapId() returns tile service URL and metadata
  mapid <- image$getMapId(reticulate::r_to_py(vis_params))

  # Extract tile URL template with robust error handling
  # Different EE API versions may use different property names
  url_template <- NULL
  if (!is.null(mapid$tile_fetcher)) {
    url_template <- mapid$tile_fetcher$url_format # New API format
  } else if (!is.null(mapid$urlFormat)) {
    url_template <- mapid$urlFormat # Alternative naming
  } else if (!is.null(mapid$tiles)) {
    url_template <- mapid$tiles[[1]] # JS-style response
  }

  # Validate successful URL extraction
  if (is.null(url_template)) {
    stop("Could not extract tile URL from Earth Engine mapid object.")
  }

  # Extract attribution text if available
  attribution <- if (!is.null(mapid$attribution)) mapid$attribution else ""

  # Build Leaflet map with base layer
  m <- leaflet::leaflet(
    options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::fitBounds(xmin, ymin, xmax, ymax)

  # Add appropriate base layer
  if (satellite_base) {
    m <- m |> leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite")
  } else {
    m <- m |> leaflet::addTiles(group = "Default")
  }

  # Add Earth Engine tile layer and layer controls
  m |>
    leaflet::addTiles(
      urlTemplate = url_template,
      attribution = attribution,
      group = layer_name,
      options = leaflet::tileOptions(opacity = 1)
    ) |>
    leaflet::addLayersControl(
      baseGroups = if (satellite_base) c("Satellite") else c("Default"),
      overlayGroups = layer_name,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
}
