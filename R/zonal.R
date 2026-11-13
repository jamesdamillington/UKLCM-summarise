# Zonal summaries of LCM rasters: land cover composition within each zone.
# exactextractr weights each pixel by the fraction of it covered by the zone,
# which matters for small urban LSOAs at 25m resolution.

# Column name for the fraction of an LCM class within a zone, e.g. "lcm_c01".
class_fraction_name <- function(class_code) {
  sprintf("lcm_c%02d", as.integer(class_code))
}

# Stop if a raster and zones do not share a CRS. Reprojecting is left to the
# caller, since choosing whether to transform zones or raster is a decision.
check_same_crs <- function(raster, zones) {
  if (!terra::same.crs(raster, sf::st_crs(zones)$wkt)) {
    stop("Raster and zones have different CRS.")
  }
  invisible(TRUE)
}

# Fraction of each LCM class within each zone.
#
# Fractions are of the valid (non-NA) area of each zone, so they sum to 1
# wherever a zone has any valid pixels, and are NA where it has none.
# NA pixels come from raster NoData (e.g. beyond LCM coverage) or from any
# masking applied to the class raster beforehand.
#
# Returns a tibble: id_field, one lcm_cNN column per class in class_codes
# (0 where a class is absent), valid_area_m2, zone_area_m2, valid_area_share.
zonal_class_fractions <- function(class_raster, zones, id_field, class_codes,
                                  progress = FALSE) {
  if (terra::nlyr(class_raster) != 1) {
    stop("class_raster must be a single-layer SpatRaster of class codes.")
  }
  check_same_crs(class_raster, zones)

  # "count" is the coverage-weighted number of non-NA cells in each zone;
  # "frac" gives one frac_<code> column per class found in any zone.
  extracted <- exactextractr::exact_extract(
    class_raster, zones,
    fun = c("count", "frac"),
    append_cols = id_field,
    progress = progress
  ) |>
    tibble::as_tibble()

  observed_codes <- names(extracted) |>
    stringr::str_subset("^frac_") |>
    stringr::str_remove("^frac_") |>
    as.integer()

  unexpected <- observed_codes |> setdiff(class_codes)
  if (length(unexpected) > 0) {
    stop("Raster contains class codes not in the scheme: ", toString(unexpected))
  }

  # Classes absent from every zone get no frac_ column from exactextractr;
  # add them as zero so output columns are the same for every run.
  absent_cols <- class_codes |>
    setdiff(observed_codes) |>
    (\(codes) paste0("frac_", codes))()
  absent_zeros <- rep(list(0), length(absent_cols)) |> purrr::set_names(absent_cols)

  cell_area_m2 <- prod(terra::res(class_raster))

  extracted |>
    tibble::add_column(!!!absent_zeros) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("frac_"),
        # Fractions are undefined for zones with no valid pixels.
        \(x) dplyr::if_else(count > 0, tidyr::replace_na(x, 0), NA_real_)
      ),
      valid_area_m2 = count * cell_area_m2,
      zone_area_m2 = as.numeric(sf::st_area(zones)),
      valid_area_share = valid_area_m2 / zone_area_m2
    ) |>
    dplyr::rename_with(
      \(x) x |> stringr::str_remove("^frac_") |> class_fraction_name(),
      dplyr::starts_with("frac_")
    ) |>
    dplyr::select(
      dplyr::all_of(id_field),
      dplyr::all_of(class_fraction_name(class_codes)),
      valid_area_m2, zone_area_m2, valid_area_share
    )
}

# Mean LCM classification confidence within each zone: coverage-weighted
# means of the probability and purity bands (0-100), as an indicator of how
# reliable each zone's composition is.
#
# Returns a tibble: id_field, mean_probability, mean_purity.
zonal_confidence_summary <- function(lcm, zones, id_field, progress = FALSE) {
  check_same_crs(lcm, zones)

  exactextractr::exact_extract(
    lcm[[c("probability", "purity")]], zones,
    fun = "mean",
    append_cols = id_field,
    progress = progress
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      mean_probability = mean.probability,
      mean_purity = mean.purity
    )
}

# Sum class fractions into groups (e.g. class_groups or expanded
# composite_groups from config). NA fractions stay NA in the sums.
#
# Returns a tibble: id_field and one <prefix><group> column per group.
summarise_class_groups <- function(class_fractions, groups, id_field,
                                   prefix = "grp_") {
  group_sums <- groups |>
    purrr::map(\(codes) {
      class_fractions |>
        dplyr::select(dplyr::all_of(class_fraction_name(codes))) |>
        rowSums()
    }) |>
    tibble::as_tibble() |>
    dplyr::rename_with(\(x) paste0(prefix, x))

  class_fractions |>
    dplyr::select(dplyr::all_of(id_field)) |>
    dplyr::bind_cols(group_sums)
}
