# Tests for R/lcm_classes.R.

test_that("lcm2021 class table has 21 classes with expected names", {
  class_table <- lcm_class_table("lcm2021_21class")

  expect_named(class_table, c("class_code", "class_name"))
  expect_equal(class_table$class_code, 1:21)
  expect_equal(
    class_table |> dplyr::filter(class_code %in% c(1, 2, 20, 21)) |> dplyr::pull(class_name),
    c("Deciduous woodland", "Coniferous woodland", "Urban", "Suburban")
  )
})

test_that("unknown class scheme raises an error", {
  expect_error(lcm_class_table("lcm1990"), "Unknown LCM class scheme")
})

test_that("valid class groups are returned unchanged", {
  groups <- list(a = 1:2, b = 3L)
  expect_identical(validate_class_groups(groups, class_codes = 1:3), groups)
})

test_that("class group validation catches gaps, overlaps and unknown codes", {
  expect_error(
    validate_class_groups(list(a = 1:2), class_codes = 1:3),
    "not in any group: 3"
  )
  expect_error(
    validate_class_groups(list(a = 1:2, b = 2:3), class_codes = 1:3),
    "more than one group: 2"
  )
  expect_error(
    validate_class_groups(list(a = 1:4), class_codes = 1:3),
    "Unknown class codes in groups: 4"
  )
})

test_that("composite groups expand to sorted class codes and may overlap", {
  class_groups <- list(a = 3L, b = 1:2, c = 4L)
  composites <- list(ab = c("a", "b"), bc = c("b", "c"))

  expanded <- expand_composite_groups(composites, class_groups)

  expect_equal(expanded$ab, 1:3)
  expect_equal(expanded$bc, c(1L, 2L, 4L))
})

test_that("composite groups referring to unknown groups raise an error", {
  expect_error(
    expand_composite_groups(list(x = c("a", "missing")), list(a = 1L)),
    "Unknown class groups in composite: missing"
  )
})

test_that("class groupings in the project config are valid", {
  # Guards against config edits that leave a class unassigned or duplicated.
  config <- testthat::test_path("..", "..", "config", "config.yml") |>
    yaml::read_yaml()
  class_codes <- lcm_class_table("lcm2021_21class")$class_code

  config$class_groupings |>
    purrr::iwalk(\(grouping, grouping_name) {
      expect_no_error(validate_class_groups(grouping$class_groups, class_codes))
      expect_no_error(expand_composite_groups(grouping$composite_groups, grouping$class_groups))
    })
})
