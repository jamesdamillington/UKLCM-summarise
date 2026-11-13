# LCM class definitions and class groupings.

# Lookup table of LCM class codes and names for a given class scheme.
# "lcm2021_21class" covers UKCEH LCM products from 2017 onwards (21 classes
# based on UKCEH Broad Habitats). Earlier LCM products (e.g. LCM2000) use
# different codes and would need their own entry here.
lcm_class_table <- function(scheme = "lcm2021_21class") {
  switch(
    scheme,
    lcm2021_21class = tibble::tibble(
      class_code = 1:21,
      class_name = c(
        "Deciduous woodland", "Coniferous woodland", "Arable",
        "Improved grassland", "Neutral grassland", "Calcareous grassland",
        "Acid grassland", "Fen", "Heather", "Heather grassland", "Bog",
        "Inland rock", "Saltwater", "Freshwater", "Supralittoral rock",
        "Supralittoral sediment", "Littoral rock", "Littoral sediment",
        "Saltmarsh", "Urban", "Suburban"
      )
    ),
    stop("Unknown LCM class scheme: '", scheme, "'.")
  )
}

# Check that class_groups (named list of class codes) assigns every class code
# exactly once, so group fractions sum to 1 within each zone.
# Returns class_groups unchanged if valid, so it can be used in a pipe.
validate_class_groups <- function(class_groups, class_codes) {
  assigned <- class_groups |> purrr::list_c()

  unknown <- assigned |> setdiff(class_codes)
  duplicated_codes <- assigned[duplicated(assigned)] |> unique()
  unassigned <- class_codes |> setdiff(assigned)

  if (length(unknown) > 0) {
    stop("Unknown class codes in groups: ", toString(unknown))
  }
  if (length(duplicated_codes) > 0) {
    stop("Class codes in more than one group: ", toString(duplicated_codes))
  }
  if (length(unassigned) > 0) {
    stop("Class codes not in any group: ", toString(unassigned))
  }

  class_groups
}

# Convert composite_groups (named list of class group names) into class codes.
# Unlike class_groups, composites may overlap (e.g. nested wildland definitions).
expand_composite_groups <- function(composite_groups, class_groups) {
  composite_groups |>
    purrr::map(\(group_names) {
      unknown <- group_names |> setdiff(names(class_groups))
      if (length(unknown) > 0) {
        stop("Unknown class groups in composite: ", toString(unknown))
      }
      class_groups[group_names] |>
        purrr::list_c() |>
        sort()
    })
}
