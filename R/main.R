# ========================================================================
# Google Satellite Embeddings Visualization Tutorial
# Clustering and change detection analysis
# ========================================================================
# Author: Milos Popovic
# ========================================================================

# ---- SECTION 1: Setup and Environment Configuration ----
# Load required packages - install if missing
install.packages("pacman")
pacman::p_load(
    reticulate, tidyverse, googledrive,
    terra, leaflet, tidyterra, grafify, patchwork
)

# ---- Python Environment Setup ----
# Check if Python is available and configure reticulate
Sys.which("python")

# Set specific Python executable for consistent behavior
# Note: Adjust this path to match your Python installation
Sys.setenv(RETICULATE_PYTHON = "C:/Users/milos/AppData/Local/Programs/Python/Python313/python.exe")
reticulate::py_config()

# Alternative setup for systems without Python installed
reticulate::install_python()
reticulate::virtualenv_create("reticulate-env")
reticulate::use_virtualenv("reticulate-env")

# ---- SECTION 2: Earth Engine Authentication and Initialization ----
# Initialize Earth Engine Python API
# Note: First time users will get browser authentication prompt
ee <- reticulate::import("ee")
ee$Authenticate()
ee$Initialize(project = "ee-your-project")

# Quick connectivity test - should return 3
connectivity_test <- ee$Number(1)$add(2)$getInfo()

# ---- SECTION 3: Area of Interest (AOI) Definition ----
# Define study region
# Coordinates format: longitude, latitude (WGS84)
xmin <- 83.919067
ymin <- 28.190059
xmax <- 83.977776
ymax <- 28.235137

# Create Earth Engine geometry object for the region
region <- ee$Geometry$Polygon(list(list(
    c(xmin, ymin), c(xmin, ymax),
    c(xmax, ymax), c(xmax, ymin)
)))

# ========================================================================
# PART I: CLUSTERING ANALYSIS
# Demonstrates unsupervised classification of satellite embeddings
# ========================================================================
# ---- SECTION 4: Satellite Embeddings Data Acquisition ----
embeddings_image <- ee$ImageCollection(
    "GOOGLE/SATELLITE_EMBEDDING/V1/ANNUAL"
)$filterDate("2024-01-01", "2025-01-01")$filterBounds(region)$mosaic()$clip(region)$toFloat()

# ---- SECTION 5: Training Data Generation for Clustering ----
# Set clustering parameters
n_samples <- 1000L

# Generate random sample points from embeddings for K-means training
training <- embeddings_image$sample(
    region = region,
    scale = 10, numPixels = n_samples,
    seed = 100L, geometries = FALSE
)

# ---- SECTION 6: K-means Clustering Function ----
get_clusters <- function(nClusters) {
    clusterer <- ee$Clusterer$wekaKMeans(
        nClusters = as.integer(nClusters)
    )$train(training)

    clustered <- embeddings_image$cluster(
        clusterer
    )
    return(clustered)
}

# Generate example clustering result for K=5
clustered_k5 <- get_clusters(5)

# ---- SECTION 7: Interactive Visualization Setup ----
# Function to display Earth Engine images in interactive Leaflet maps
devtools::source_url("https://raw.githubusercontent.com/milos-agathon/visualize-google-satellite-embeddings/refs/heads/main/R/ee_leaflet_map.r")

# ---- SECTION 8: Interactive Map Display ----
# Define visualization parameters for K=5 clustering
kelly <- grafify::graf_palettes[["kelly"]]

vis_k5 <- list(
    min = 0, max = 4, center = c(xmin, ymin),
    palette = kelly[c(1:5)]
)

# Create and display interactive map
map_k5 <- ee_leaflet_map(
    clustered_k5$toInt(),
    vis_params = vis_k5,
    zoom = 13, satellite_base = TRUE,
    layer_name = "K=5 clusters"
)

map_k5

# ---- SECTION 9: Batch Export and Download Functions ----
# Authenticate with Google Drive (same account as Earth Engine)
googledrive::drive_auth()
googledrive::drive_mkdir("earthengine-exports")
export_folder <- googledrive::drive_get(
    "earthengine-exports"
)
drive_files <- googledrive::drive_ls(
    export_folder
)
print(drive_files)

# Function to export clustering results to Google Drive
export_raster <- function(k) {
    clustered <- get_clusters(k)
    clustered_int <- clustered$toInt()$clip(region)
    # Configure export parameters
    description <- sprintf(
        "clusters_k%d_export", k
    )
    filename_prefix <- sprintf(
        "clusters_k%d", k
    )

    # Create export task to Google Drive
    task <- ee$batch$Export$image$toDrive(
        image = clustered_int,
        description = description,
        folder = "earthengine-exports",
        fileNamePrefix = filename_prefix,
        scale = 10, region = region,
        fileFormat = "GeoTIFF", maxPixels = 1e13 # fileFormat
    )

    task$start()
}

# ---- SECTION 10: Batch Processing Multiple K Values ----
# Define range of K values to analyze

cluster_values <- c(3L, 5L, 10L)

# Export all clustering results to Google Drive
# Note: Wait for exports to complete before running download section
lapply(cluster_values, export_raster)

# ---- SECTION 11: Download and Visualization ----
# Function to download exported files from Google Drive
download_raster <- function(k, fs) {
    filename_prefix <- sprintf(
        "clusters_k%d", k
    )

    pattern <- paste0("^", filename_prefix)

    # Search for matching files in Drive folder
    matching_files <- fs |>
        filter(grepl(pattern, name))

    if (nrow(matching_files) == 0) {
        stop(
            "could not find exported file for K=",
            k, " in Drive folder."
        )
    }

    # Prefer .tif over .zip if both formats exist
    chosen_file <- matching_files |>
        arrange(
            desc(grepl("\\.tif$", name))
        ) |>
        slice(1)

    # Download file with original name
    local_filename <- chosen_file$name
    googledrive::drive_download(
        chosen_file,
        path = local_filename,
        overwrite = TRUE
    )
}

# Download all clustering results
lapply(
    cluster_values,
    function(k) {
        download_raster(
            k, drive_files
        )
    }
)

# ---- SECTION 12: Multi-panel Visualization Creation ----
# Load downloaded raster files into R
cluster_filenames <- drive_files$name[grepl(
    "cluster", drive_files$name
)]

cluster_rasters <- lapply(
    cluster_filenames, terra::rast
)

names(cluster_rasters) <- paste0(
    "K", c(5, 10, 3)
)

# Function to create standardized cluster plots
make_cluster_plot <- function(
    r, title, show_legend = FALSE) {
  ggplot() +
    tidyterra::geom_spatraster(
      data = as.factor(r)
    ) + # Convert to factors for discrete colors
    scale_fill_grafify(
      palette = "kelly", # Use Kelly palette for maximum distinction
      drop = FALSE, # Keep all factor levels
      guide = "none" # Hide individual legends
    ) +
    labs(title = title) + # Add descriptive title
    theme_minimal() # Clean map appearance
}
# Generate individual plots for each K value
individual_plots <- mapply(
    FUN = make_cluster_plot,
    r = cluster_rasters,
    title = paste0("K = ", c(10, 3, 5)),
    show_legend = c(FALSE, FALSE, FALSE),
    SIMPLIFY = FALSE
)

# Combine plots into single panel using patchwork
# Layout: horizontal arrangement with shared legend
combined_plot <- (
    individual_plots[[2]] +
    individual_plots[[3]] +
    individual_plots[[1]] +
    plot_layout(nrow = 1, guides = "collect")
)

# ========================================================================
# PART II: CHANGE DETECTION ANALYSIS
# Demonstrates temporal analysis using satellite embeddings
# ========================================================================
# ---- SECTION 13: Multi-temporal Data Acquisition ----
# Function to retrieve satellite embeddings for specific year
get_satellite_embeddings <- function(y0, y1) {
    ee$ImageCollection(
        "GOOGLE/SATELLITE_EMBEDDING/V1/ANNUAL"
    )$filterDate(
        sprintf("%d-01-01", y0),
        sprintf("%d-01-01", y1)
    )$filterBounds(region)$mosaic()$clip(region)$toFloat()
}

# Generic Earth Engine export function
ee_export <- function(
    image, filename, region, scale) {
    task <- ee$batch$Export$image$toDrive(
        image = image,
        folder = "earthengine-exports",
        fileNamePrefix = tools::file_path_sans_ext(
            basename(filename)
        ),
        region = region, scale = as.integer(scale),
        maxPixels = 1e13
    )
    task$start()
}

# ---- SECTION 14: Temporal Data Export ----
# Export 2018 embeddings if not already downloaded
ee_export(get_satellite_embeddings(
    2018, 2019
), "embeddings_2018.tif", region, 10)

ee_export(get_satellite_embeddings(
    2024, 2025
), "embeddings_2024.tif", region, 10)

# ---- SECTION 15: Temporal Data Download ----
# Download temporal embeddings from Google Drive
export_folder <- googledrive::drive_get(
    "earthengine-exports"
)

all_files <- googledrive::drive_ls(
    export_folder
)

embeddings_files <- all_files$name[grepl(
    "embeddings", all_files$name
)]

# Download each embeddings file
lapply(
    embeddings_files, function(filename) {
        googledrive::drive_download(
            filename,
            path = filename,
            overwrite = TRUE
        )
    }
)

# ---- SECTION 16: Change Detection Analysis ----
# Note: Assumes 2018 file is first, 2024 file is second
# Adjust indexing if files are in different order
embeddings_2018 <- terra::rast(embeddings_files[[1]])
embeddings_2024 <- terra::rast(embeddings_files[[2]])

# Simple difference analysis
# Calculate absolute difference between years
difference_raster <- embeddings_2024 - embeddings_2018
mean_absolute_difference <- mean(abs(difference_raster))

# Visualize mean absolute difference
difference_plot <- ggplot() +
    tidyterra::geom_spatraster(
        data = mean_absolute_difference
    ) +
    scale_fill_viridis_c(
        name = "Mean diff.", option = "magma"
    ) +
    theme_minimal() +
    theme(axis.text = element_text(size = 8))

ggsave(
    "mean_absolute_difference.png",
    difference_plot,
    width = 7.5, height = 6,
    dpi = 600, bg = "white"
)

# ---- SECTION 17: Cosine Similarity Analysis ----
# Advanced change detection using cosine similarity
# Cosine similarity measures directional change in embedding space
# Values near 1 = similar, values near 0 = different, values near -1 = opposite
combined_embeddings <- c(embeddings_2018, embeddings_2024)

# Function to compute cosine similarity between embedding vectors
# Applied pixel-by-pixel across the raster
cosine_similarity_function <- function(v) {
    embeddings_2018_pixel <- v[, 1:64, drop = FALSE]
    embeddings_2024_pixel <- v[, 65:128, drop = FALSE]

    # Compute dot product (numerator)
    dot_product <- rowSums(
        embeddings_2018_pixel * embeddings_2024_pixel
    )

    # Compute magnitudes (denominator)
    magnitude_2018 <- sqrt(rowSums(embeddings_2018_pixel^2))
    magnitude_2024 <- sqrt(rowSums(embeddings_2024_pixel^2))

    # Calculate cosine similarity with bounds checking
    cosine_values <- dot_product / (magnitude_2018 * magnitude_2024)
    pmin(pmax(cosine_values, -1), 1)
}

# Apply cosine similarity function across all pixels
# terra::app() applies function pixel-by-pixel across raster stack
cosine_similarity_raster <- terra::app(
    combined_embeddings, cosine_similarity_function,
    filename = "cosine_2018_2024.tif", # save locally
    wopt = list(datatype = "FLT4S", overwrite = TRUE)
)

# ---- SECTION 18: Change Visualization ----
# Create change detection visualization
# Lower cosine similarity indicates greater change between years
change_plot <- ggplot() +
    tidyterra::geom_spatraster(
        data = cosine_similarity_raster
    ) +
    scale_fill_viridis_c(
        name = "Cosine similarity",
        option = "magma", direction = -1
    ) +
    labs(
        title = "Land Cover Change: 2018 → 2024",
        subtitle = "Lower similarity indicates greater change"
    ) +
    theme_minimal() +
    theme(
        axis.text = element_text(size = 8),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "bold"),
    )

# Save high-resolution change map
ggsave(
    "cosine_change_heatmap.png",
    change_plot,
    width = 7.5,
    height = 6, dpi = 600, bg = "white"
)
