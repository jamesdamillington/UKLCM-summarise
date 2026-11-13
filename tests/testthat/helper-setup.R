# Shared setup for unit tests. testthat runs helper-*.R files before tests.
# Run all tests from the project root with:
#   testthat::test_dir("tests/testthat")

# Source project functions.
testthat::test_path("..", "..", "R") |>
  list.files(pattern = "\\.R$", full.names = TRUE) |>
  purrr::walk(source)

# A 4x4 raster of 25m cells (British National Grid) with known class codes.
# Layout, top row first:
#    1  1 20 20
#    1  1 20 20
#    4  4 21 NA
#    4  4 21 NA
make_test_class_raster <- function() {
  r <- terra::rast(
    nrows = 4, ncols = 4,
    xmin = 0, xmax = 100, ymin = 0, ymax = 100,
    crs = "EPSG:27700"
  )
  terra::values(r) <- c(
    1, 1, 20, 20,
    1, 1, 20, 20,
    4, 4, 21, NA,
    4, 4, 21, NA
  )
  names(r) <- "class"
  r
}

# Three test zones over the raster above:
#   left    - left half: 4 cells class 1, 4 cells class 4
#   right   - right half: 4 cells class 20, 2 cells class 21, 2 NA cells
#   na_only - bottom-right quarter column: NA cells only
make_test_zones <- function() {
  square <- function(xmin, ymin, xmax, ymax) {
    c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin) |>
      matrix(ncol = 2, byrow = TRUE) |>
      list() |>
      sf::st_polygon()
  }

  sf::st_sf(
    zone_id = c("left", "right", "na_only"),
    geometry = sf::st_sfc(
      square(0, 0, 50, 100),
      square(50, 0, 100, 100),
      square(75, 0, 100, 50),
      crs = 27700
    )
  )
}
