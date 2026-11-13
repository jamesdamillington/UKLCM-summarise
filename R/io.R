# Reading input data. These functions return objects and never write files.

# Read a UKCEH LCM 25m raster and name its bands.
# LCM 25m rasters have three bands:
#   1 = most likely land cover class
#   2 = per-parcel probability of that class (0-100)
#   3 = parcel purity (0-100)
# NoData is 0 in the source files, which terra reads as NA.
read_lcm <- function(path, band_names = c("class", "probability", "purity")) {
  lcm <- terra::rast(path)

  if (terra::nlyr(lcm) != length(band_names)) {
    stop("Expected ", length(band_names), " bands in ", path,
         " but found ", terra::nlyr(lcm), ".")
  }

  names(lcm) <- band_names
  lcm
}

# Read zone polygons (e.g. LSOAs), keeping only the identifier and geometry
# so downstream code doesn't depend on source-specific attribute fields.
read_zones <- function(path, id_field) {
  zones <- sf::st_read(path, quiet = TRUE)

  if (!id_field %in% names(zones)) {
    stop("Field '", id_field, "' not found in ", path, ".")
  }
  if (anyNA(zones[[id_field]]) || anyDuplicated(zones[[id_field]])) {
    stop("Zone id field '", id_field, "' has missing or duplicate values.")
  }

  zones <- dplyr::select(zones, dplyr::all_of(id_field))

  # The London LSOA file has at least one invalid geometry, which can break
  # zonal extraction. st_make_valid() leaves valid geometries unchanged.
  sf::st_make_valid(zones)
}

# Read the settings for one run from the project config file.
#
# Returns a flat list: run_name, the run's own settings (lcm_year, lcm_path,
# lcm_scheme, zones_path, zone_id_field, class_grouping), the shared output
# paths (derived_dir, output_dir), plus class_groups and composite_groups
# from the run's named class grouping.
# run = NULL uses the config's default_run.
read_run_config <- function(config_path = "config/config.yml", run = NULL) {
  config <- yaml::read_yaml(config_path)
  run <- run %||% config$default_run

  if (!run %in% names(config$runs)) {
    stop("Run '", run, "' not found in ", config_path,
         ". Available runs: ", toString(names(config$runs)), ".")
  }
  run_settings <- config$runs[[run]]

  grouping_name <- run_settings$class_grouping
  if (!grouping_name %in% names(config$class_groupings)) {
    stop("Class grouping '", grouping_name, "' not found in ", config_path, ".")
  }
  grouping <- config$class_groupings[[grouping_name]]

  list(run_name = run) |>
    c(run_settings) |>
    c(config$paths) |>
    c(grouping[c("class_groups", "composite_groups")])
}
