# Tests for R/io.R. Each test writes small input files to a temporary
# directory, then reads them back with the project functions.

# Write a 3-band raster shaped like an LCM file (class, probability, purity),
# with 0 as NoData as in the UKCEH source files. Returns the file path.
write_test_lcm <- function(n_bands = 3) {
  path <- tempfile(fileext = ".tif")
  class_raster <- make_test_class_raster()
  bands <- rep(list(class_raster), n_bands) |> terra::rast()

  bands |>
    terra::subst(NA, 0) |>
    terra::writeRaster(path, datatype = "INT1U", NAflag = 0)
  path
}

# Write zones to a temporary GeoPackage. Returns the file path.
write_test_zones <- function(zones) {
  path <- tempfile(fileext = ".gpkg")
  sf::st_write(zones, path, quiet = TRUE)
  path
}

# ---- read_lcm ---------------------------------------------------------------

test_that("read_lcm names the three LCM bands", {
  lcm <- read_lcm(write_test_lcm())

  expect_s4_class(lcm, "SpatRaster")
  expect_equal(names(lcm), c("class", "probability", "purity"))
})

test_that("read_lcm reads NoData (0) as NA", {
  lcm <- read_lcm(write_test_lcm())
  class_values <- terra::values(lcm[["class"]])

  expect_equal(sum(is.na(class_values)), 2)
  expect_false(any(class_values == 0, na.rm = TRUE))
})

test_that("read_lcm errors when band count does not match band names", {
  expect_error(read_lcm(write_test_lcm(n_bands = 2)), "Expected 3 bands")
})

# ---- read_zones -------------------------------------------------------------

test_that("read_zones keeps only the id field and geometry", {
  zones <- make_test_zones() |>
    dplyr::mutate(other_field = "drop me")

  result <- read_zones(write_test_zones(zones), id_field = "zone_id")

  expect_s3_class(result, "sf")
  expect_equal(setdiff(names(result), attr(result, "sf_column")), "zone_id")
  expect_equal(result$zone_id, zones$zone_id)
})

test_that("read_zones errors on a missing id field", {
  path <- write_test_zones(make_test_zones())
  expect_error(read_zones(path, id_field = "LSOA21CD"), "not found")
})

test_that("read_zones errors on duplicate or missing ids", {
  duplicated_ids <- make_test_zones() |>
    dplyr::mutate(zone_id = c("a", "a", "b"))
  missing_ids <- make_test_zones() |>
    dplyr::mutate(zone_id = c("a", NA, "b"))

  expect_error(
    read_zones(write_test_zones(duplicated_ids), "zone_id"),
    "missing or duplicate"
  )
  expect_error(
    read_zones(write_test_zones(missing_ids), "zone_id"),
    "missing or duplicate"
  )
})

test_that("read_zones repairs invalid geometries", {
  # A self-intersecting "bowtie" polygon is invalid.
  bowtie <- c(0, 0, 100, 100, 100, 0, 0, 100, 0, 0) |>
    matrix(ncol = 2, byrow = TRUE) |>
    list() |>
    sf::st_polygon()
  zones <- sf::st_sf(
    zone_id = "bowtie",
    geometry = sf::st_sfc(bowtie, crs = 27700)
  )
  expect_false(sf::st_is_valid(zones))

  result <- read_zones(write_test_zones(zones), "zone_id")

  expect_true(all(sf::st_is_valid(result)))
  expect_equal(nrow(result), 1)
})

# ---- read_run_config ----------------------------------------------------------

# Write a minimal config with two runs and one class grouping. Returns the path.
write_test_config <- function() {
  path <- tempfile(fileext = ".yml")
  list(
    default_run = "run_a",
    paths = list(derived_dir = "derived", output_dir = "out"),
    runs = list(
      run_a = list(lcm_year = 2022L, zone_id_field = "zone_id", class_grouping = "g1"),
      run_b = list(lcm_year = 2015L, zone_id_field = "zone_id", class_grouping = "g1"),
      run_bad = list(lcm_year = 2022L, zone_id_field = "zone_id", class_grouping = "missing")
    ),
    class_groupings = list(
      g1 = list(
        class_groups = list(a = 1:2, b = 3L),
        composite_groups = list(ab = c("a", "b"))
      )
    )
  ) |>
    yaml::write_yaml(path)
  path
}

test_that("read_run_config uses the default run when none is given", {
  config <- read_run_config(write_test_config())

  expect_equal(config$run_name, "run_a")
  expect_equal(config$lcm_year, 2022L)
})

test_that("read_run_config returns the requested run with its class grouping", {
  config <- read_run_config(write_test_config(), run = "run_b")

  expect_equal(config$run_name, "run_b")
  expect_equal(config$lcm_year, 2015L)
  expect_equal(config$output_dir, "out")
  expect_equal(config$class_groups, list(a = 1:2, b = 3L))
  expect_equal(config$composite_groups, list(ab = c("a", "b")))
})

test_that("read_run_config errors on unknown runs or class groupings", {
  path <- write_test_config()

  expect_error(read_run_config(path, run = "run_z"), "Run 'run_z' not found")
  expect_error(read_run_config(path, run = "run_bad"), "grouping 'missing' not found")
})

test_that("the project config's default run has the settings the pipeline needs", {
  config <- testthat::test_path("..", "..", "config", "config.yml") |>
    read_run_config()

  expect_true(all(
    c("run_name", "lcm_year", "lcm_path", "lcm_scheme", "zones_path",
      "zone_id_field", "derived_dir", "output_dir", "class_groups",
      "composite_groups") %in% names(config)
  ))
})
