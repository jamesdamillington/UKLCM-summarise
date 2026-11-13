# Pipeline: LCM raster -> land cover composition per zone.
#
# Run from the project root:
#   targets::tar_make()               # build (only out-of-date steps rerun)
#   targets::tar_read(zone_composition)
#   targets::tar_visnetwork()         # dependency graph
#
# To build a run other than the config's default_run:
#   Sys.setenv(UKLCM_RUN = "run_name"); targets::tar_make()
#
# Current stage: aspatial composition of each zone. Pixel-level neighbourhood
# measures (wildland share within a radius, distance to large patches) will be
# added later as further targets computed wall-to-wall, then summarised to zones.

library(targets)

tar_option_set(
  packages = c("terra", "sf", "exactextractr", "dplyr", "purrr")
)

tar_source("R")

# Which run to build. targets tracks this global, so changing it invalidates
# downstream targets. All runs share one _targets/ store for now; building
# several years/regions side by side (Phase 2) would use tar_map() from the
# tarchetypes package (not currently a dependency).
selected_run <- Sys.getenv("UKLCM_RUN", unset = "") |>
  (\(x) if (x == "") NULL else x)()

list(
  # ---- Configuration ---------------------------------------------------------
  tar_target(config_file, "config/config.yml", format = "file"),
  tar_target(run_config, read_run_config(config_file, run = selected_run)),
  tar_target(zone_id_field, run_config$zone_id_field),
  tar_target(class_codes, lcm_class_table(run_config$lcm_scheme)$class_code),
  tar_target(
    class_groups,
    run_config$class_groups |> validate_class_groups(class_codes)
  ),
  tar_target(
    composite_groups,
    run_config$composite_groups |> expand_composite_groups(class_groups)
  ),

  # ---- Inputs ----------------------------------------------------------------
  # format = "file" makes targets rerun downstream steps if the file changes.
  # For the shapefile only the .shp is tracked; edits to sidecar files alone
  # (.dbf, .prj) will not trigger a rebuild.
  tar_target(lcm_source_file, run_config$lcm_path, format = "file"),
  tar_target(zones_source_file, run_config$zones_path, format = "file"),

  # One-off conversion of the strip-organised source raster to a tiled copy.
  tar_target(
    lcm_file,
    read_lcm(lcm_source_file) |>
      write_tiled_raster(
        file.path(run_config$derived_dir, run_config$run_name, "lcm_tiled.tif")
      ),
    format = "file"
  ),
  tar_target(zones, read_zones(zones_source_file, zone_id_field)),

  # ---- Zonal composition -----------------------------------------------------
  # SpatRasters hold pointers to external memory and cannot be saved as targets,
  # so each step reads the raster from file.
  tar_target(
    class_fractions,
    read_lcm(lcm_file)[["class"]] |>
      zonal_class_fractions(zones, zone_id_field, class_codes)
  ),
  tar_target(
    zone_confidence,
    read_lcm(lcm_file) |>
      zonal_confidence_summary(zones, zone_id_field)
  ),
  tar_target(
    group_fractions,
    class_fractions |>
      summarise_class_groups(class_groups, zone_id_field, prefix = "grp_")
  ),
  tar_target(
    composite_fractions,
    class_fractions |>
      summarise_class_groups(composite_groups, zone_id_field, prefix = "cmp_")
  ),
  tar_target(
    zone_composition,
    list(class_fractions, group_fractions, composite_fractions, zone_confidence) |>
      purrr::reduce(dplyr::left_join, by = zone_id_field)
  ),

  # ---- Outputs ---------------------------------------------------------------
  tar_target(
    zone_composition_csv,
    zone_composition |>
      write_zone_table(
        file.path(run_config$output_dir, run_config$run_name, "zone_composition.csv")
      ),
    format = "file"
  )
)
