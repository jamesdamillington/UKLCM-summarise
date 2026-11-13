# Tests for R/write.R. Files are written to a temporary directory.

# ---- write_zone_table -------------------------------------------------------

test_that("write_zone_table writes a CSV that reads back unchanged", {
  zone_table <- tibble::tibble(zone_id = c("a", "b"), lcm_c01 = c(0.25, 0.75))
  path <- file.path(tempfile(), "nested", "zone_table.csv")

  returned_path <- write_zone_table(zone_table, path)
  read_back <- readr::read_csv(path, show_col_types = FALSE)

  expect_equal(returned_path, path)
  expect_true(file.exists(path)) # nested directory was created
  # read_csv() adds its own tibble subclass, so compare as plain data frames.
  expect_equal(as.data.frame(read_back), as.data.frame(zone_table))
})

test_that("write_zone_table refuses sf objects", {
  expect_error(
    write_zone_table(make_test_zones(), tempfile(fileext = ".csv")),
    "drop geometry"
  )
})

# ---- write_tiled_raster -----------------------------------------------------

test_that("write_tiled_raster preserves values and NoData", {
  raster <- make_test_class_raster()
  path <- file.path(tempfile(), "tiled.tif")

  returned_path <- write_tiled_raster(raster, path)
  read_back <- terra::rast(path)

  expect_equal(returned_path, path)
  expect_equal(terra::values(read_back), terra::values(raster))
  expect_equal(sum(is.na(terra::values(read_back))), 2)
})

test_that("write_tiled_raster writes 256x256 tiles", {
  path <- write_tiled_raster(make_test_class_raster(), tempfile(fileext = ".tif"))
  gdal_info <- sf::gdal_utils("info", path, quiet = TRUE)

  expect_match(gdal_info, "Block=256x256")
})
