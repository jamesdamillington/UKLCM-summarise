# Sanity checks on the zone composition produced by the pipeline.
#
# Unlike the unit tests (synthetic data, pass/fail), this script prints
# summaries of the real results for a person to judge whether they make sense.
# Run after targets::tar_make(), from the project root, line by line or with:
#   source("analysis/check_zone_composition.R")

library(dplyr)
library(tidyr)

targets::tar_source("R") # for lcm_class_table() and class_fraction_name()

composition <- targets::tar_read(zone_composition)
run_config <- targets::tar_read(run_config)
id_field <- run_config$zone_id_field

class_table <- lcm_class_table(run_config$lcm_scheme) |>
  mutate(column = class_fraction_name(class_code))

cat("Run:", run_config$run_name, "\n")
cat("Zones:", nrow(composition), " Columns:", ncol(composition), "\n\n")

# ---- 1. Completeness ---------------------------------------------------------
# Expect no NAs unless a zone lies entirely outside LCM coverage.
cat("Columns containing NA:\n")
composition |>
  summarise(across(everything(), \(x) sum(is.na(x)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "n_na") |>
  filter(n_na > 0) |>
  print()

# ---- 2. Class fractions sum to 1 ---------------------------------------------
cat("\nRange of class fraction row sums (expect 1, within float precision):\n")
composition |>
  select(all_of(class_table$column)) |>
  rowSums() |>
  range() |>
  print()

# ---- 3. Valid area -----------------------------------------------------------
# valid_area_share below 1 means part of the zone has no LCM data, or the zone
# boundary and raster extent differ slightly. Large shortfalls need a look.
cat("\nvalid_area_share summary:\n")
composition$valid_area_share |> summary() |> print()

cat("\nZones with valid_area_share < 0.99:\n")
composition |>
  filter(valid_area_share < 0.99) |>
  select(all_of(id_field), valid_area_share, zone_area_m2) |>
  arrange(valid_area_share) |>
  print()

# ---- 4. Land cover across the whole study area -------------------------------
# Area-weighted totals: does the overall mix look plausible for the region?
cat("\nTotal area by LCM class (km2) across all zones:\n")
composition |>
  select(valid_area_m2, all_of(class_table$column)) |>
  pivot_longer(-valid_area_m2, names_to = "column", values_to = "fraction") |>
  group_by(column) |>
  summarise(area_km2 = sum(fraction * valid_area_m2) / 1e6) |>
  left_join(class_table, by = "column") |>
  mutate(share = area_km2 / sum(area_km2)) |>
  arrange(desc(area_km2)) |>
  select(class_code, class_name, area_km2, share) |>
  print(n = Inf)

# ---- 5. Composite group distributions ----------------------------------------
cat("\nComposite groups: quantiles and share of zones with any cover:\n")
composition |>
  select(starts_with("cmp_")) |>
  pivot_longer(everything(), names_to = "composite", values_to = "fraction") |>
  group_by(composite) |>
  summarise(
    p10 = quantile(fraction, 0.10, na.rm = TRUE),
    median = median(fraction, na.rm = TRUE),
    p90 = quantile(fraction, 0.90, na.rm = TRUE),
    max = max(fraction, na.rm = TRUE),
    share_zones_nonzero = mean(fraction > 0, na.rm = TRUE)
  ) |>
  print()

# ---- 6. Classification confidence --------------------------------------------
cat("\nMean probability and purity across zones:\n")
composition |>
  select(mean_probability, mean_purity) |>
  summary() |>
  print()

cat("\nTen zones with the lowest mean probability:\n")
composition |>
  arrange(mean_probability) |>
  select(all_of(id_field), mean_probability, mean_purity, cmp_built, cmp_wildland_broad) |>
  head(10) |>
  print()
