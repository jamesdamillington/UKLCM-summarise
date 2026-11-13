# Explicit file writers. Analytical functions elsewhere return objects; only
# the functions here write to disk. Each returns the path it wrote, so it can
# be tracked as a file output in a pipeline.

# Write a zone-level table (e.g. class fractions per LSOA) to CSV,
# creating the output directory if needed.
write_zone_table <- function(zone_table, path) {
  if (inherits(zone_table, "sf")) {
    stop("zone_table is an sf object; drop geometry before writing to CSV.")
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  zone_table |> readr::write_csv(path)
  path
}

# Write a raster as an internally tiled, compressed GeoTIFF.
#
# The UKCEH LCM distribution is stored in single-row strips, which makes
# reading small windows (per-zone extraction, cropping around a zone) slow.
# 256x256 tiles allow fast windowed reads. Values are stored as unsigned
# 8-bit integers, which suits LCM class codes and the 0-100 confidence bands,
# with 0 as NoData to match the source files.
write_tiled_raster <- function(raster, path, overwrite = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  raster |>
    terra::writeRaster(
      path,
      datatype = "INT1U",
      NAflag = 0,
      gdal = c("TILED=YES", "BLOCKXSIZE=256", "BLOCKYSIZE=256", "COMPRESS=DEFLATE"),
      overwrite = overwrite
    )
  path
}
