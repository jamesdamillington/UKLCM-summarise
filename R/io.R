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
