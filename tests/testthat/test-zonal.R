# Tests for R/zonal.R using the synthetic raster and zones in helper-setup.R.
# exactextractr returns fractions in single precision, hence the tolerances.

test_that("class fractions match the known raster layout", {
  fractions <- zonal_class_fractions(
    make_test_class_raster(), make_test_zones(),
    id_field = "zone_id", class_codes = 1:21
  )
  left <- fractions |> dplyr::filter(zone_id == "left")
  right <- fractions |> dplyr::filter(zone_id == "right")

  expect_equal(left$lcm_c01, 0.5, tolerance = 1e-6)
  expect_equal(left$lcm_c04, 0.5, tolerance = 1e-6)
  expect_equal(right$lcm_c20, 4 / 6, tolerance = 1e-6)
  expect_equal(right$lcm_c21, 2 / 6, tolerance = 1e-6)
})

test_that("output has one column per class, absent classes are zero", {
  fractions <- zonal_class_fractions(
    make_test_class_raster(), make_test_zones(),
    id_field = "zone_id", class_codes = 1:21
  )
  left <- fractions |> dplyr::filter(zone_id == "left")

  expect_true(all(class_fraction_name(1:21) %in% names(fractions)))
  # Class 2 occurs in no zone; class 20 occurs, but not in the left zone.
  expect_equal(left$lcm_c02, 0)
  expect_equal(left$lcm_c20, 0)
})

test_that("fractions sum to 1 and NA pixels are excluded", {
  fractions <- zonal_class_fractions(
    make_test_class_raster(), make_test_zones(),
    id_field = "zone_id", class_codes = 1:21
  )
  with_data <- fractions |> dplyr::filter(zone_id != "na_only")
  right <- fractions |> dplyr::filter(zone_id == "right")

  row_sums <- with_data |>
    dplyr::select(dplyr::starts_with("lcm_c")) |>
    rowSums()
  expect_equal(unname(row_sums), c(1, 1), tolerance = 1e-6)

  # Right zone: 6 valid cells of 625 m2 out of 8.
  expect_equal(right$valid_area_m2, 6 * 625)
  expect_equal(right$valid_area_share, 0.75)
})

test_that("zones with no valid pixels get NA fractions", {
  fractions <- zonal_class_fractions(
    make_test_class_raster(), make_test_zones(),
    id_field = "zone_id", class_codes = 1:21
  )
  na_only <- fractions |> dplyr::filter(zone_id == "na_only")

  expect_true(is.na(na_only$lcm_c01))
  expect_equal(na_only$valid_area_m2, 0)
})

test_that("invalid inputs raise errors", {
  raster <- make_test_class_raster()
  zones <- make_test_zones()

  # Class codes outside the scheme
  expect_error(
    zonal_class_fractions(raster, zones, "zone_id", class_codes = 1:5),
    "not in the scheme"
  )
  # Multi-layer raster
  expect_error(
    zonal_class_fractions(c(raster, raster), zones, "zone_id", 1:21),
    "single-layer"
  )
  # CRS mismatch
  expect_error(
    zonal_class_fractions(raster, sf::st_transform(zones, 4326), "zone_id", 1:21),
    "different CRS"
  )
})

test_that("confidence summary gives coverage-weighted means", {
  class_layer <- make_test_class_raster()
  probability <- terra::setValues(class_layer, rep(c(80, 40), 8)) # alternates by column
  purity <- terra::setValues(class_layer, 100)
  lcm <- c(class_layer, probability, purity) |>
    stats::setNames(c("class", "probability", "purity"))

  confidence <- zonal_confidence_summary(lcm, make_test_zones(), "zone_id")
  left <- confidence |> dplyr::filter(zone_id == "left")

  expect_named(confidence, c("zone_id", "mean_probability", "mean_purity"))
  expect_equal(left$mean_probability, 60) # half 80, half 40
  expect_equal(left$mean_purity, 100)
})

test_that("group sums add the right class fractions", {
  fractions <- zonal_class_fractions(
    make_test_class_raster(), make_test_zones(),
    id_field = "zone_id", class_codes = 1:21
  )
  groups <- list(built = c(20L, 21L), green = c(1L, 4L))

  grouped <- fractions |> summarise_class_groups(groups, "zone_id", prefix = "grp_")

  expect_named(grouped, c("zone_id", "grp_built", "grp_green"))
  expect_equal(grouped$grp_green[grouped$zone_id == "left"], 1, tolerance = 1e-6)
  expect_equal(grouped$grp_built[grouped$zone_id == "right"], 1, tolerance = 1e-6)
  expect_true(is.na(grouped$grp_built[grouped$zone_id == "na_only"]))

  # Unknown class codes in a group should fail rather than silently drop.
  expect_error(summarise_class_groups(fractions, list(bad = 99L), "zone_id"))
})
