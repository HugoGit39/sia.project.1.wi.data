########################################################################
#
# Purpose:
#   Build a single, Shiny-ready table (one row per device) by
#   reading raw Excels, pivoting long→wide where needed, and
#   joining everything on `device_id`.
#
# Inputs (from data/raw/):
#   - devices.xlsx
#   - signals.xlsx              (long: per-signal details)
#   - technical_specs.xlsx      (long: spec_* fields)
#   - data_access.xlsx          (long: spec_* fields)
#   - rvu_synthesis.xlsx        (long: synthesis per type)
#   - expert_scores.xlsx        (long: score_type x reviewer)
#
# Outputs (to output/data/):
#   - df_shiny_wi.csv
#   - df_shiny_wi.xlsx
#
# Method (TL;DR):
#   1) read_xlsx() + clean_names()
#   2) use norm_key() to create stable, machine-friendly keys
#      (avoids janitor’s _2 suffix across devices)
#   3) long → wide (in build order):
#        - scores:       {score_type}_{reviewer}_score
#        - specs:        {spec}_{boel|num_value|num_unit|char_value}
#        - signals:      {signal}_{available|sampling_rate_min|max|additional_info|recording_location}
#        - data_access:  {spec}_{boel|num_value|num_unit|char_value}
#        - rvu:          {synthesis_type}_{n_of_studies|evidence_level|parameters_studied|synthesis|date_of_last_search}
#   4) left_join(...) by device_id into df_shiny_wi
#   5) write CSV + XLSX
#
# Notes:
#   - `device_id` should be consistent across sheets (character).
#   - Numeric coercions use suppressWarnings(); Inf from empty mins/maxes → NA.
#
# Stress in Action 2025
########################################################################

# * 1  read raw data ----
source(here("src","application","sia.p1.read.data.R"))

# * 2  functions ----
source(here("src","function","sai.p1.functions.R"))

# * 3 expert scores (long -> wide) ----
df_shiny_scores <- scores %>%
  mutate(
    score_key    = norm_key(score_type),
    reviewer_key = norm_key(score_by),
    score        = suppressWarnings(as.numeric(score))
  ) %>%
  group_by(device_id, score_key, reviewer_key) %>%
  summarise(score = mean(score, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = device_id,
    names_from = c(score_key, reviewer_key),
    values_from = score,
    names_glue = "{score_key}_{reviewer_key}_score"
  )

# * 4 devices df ----
df_shiny_devices <- read_xlsx(p_devices) 

# * 5 technical specs (long -> wide) ----
df_shiny_specs <- specs %>%
  mutate(
    spec_key       = norm_key(spec_name),
    spec_num_value = suppressWarnings(as.numeric(spec_num_value))
  ) %>%
  group_by(device_id, spec_key) %>%
  summarise(
    spec_boel_value = first(na.omit(spec_boel_value), default = NA),
    spec_num_value  = suppressWarnings(max(spec_num_value, na.rm = TRUE)),
    spec_num_unit   = first(na.omit(spec_num_unit),  default = NA),
    spec_char_value = first(na.omit(spec_char_value), default = NA),
    .groups = "drop"
  ) %>%
  mutate(
    spec_num_value = ifelse(is.infinite(spec_num_value), NA_real_, spec_num_value)
  ) %>%
  pivot_wider(
    id_cols = device_id,
    names_from = spec_key,
    values_from = c(spec_boel_value, spec_num_value, spec_num_unit, spec_char_value),
    names_glue = "{spec_key}_{.value}"
  )

# * 6 signals df (long -> wide) ----
df_shiny_signals <- signals_long %>%
  mutate(
    signal_key         = norm_key(signal_name),
    sampling_rate_min  = suppressWarnings(as.numeric(sampling_rate_min)),
    sampling_rate_max  = suppressWarnings(as.numeric(sampling_rate_max))
  ) %>%
  group_by(device_id, signal_key) %>%
  summarise(
    available           = first(na.omit(available)),
    sampling_rate_min   = suppressWarnings(min(sampling_rate_min, na.rm = TRUE)),
    sampling_rate_max   = suppressWarnings(max(sampling_rate_max, na.rm = TRUE)),
    additional_info     = first(na.omit(additional_info)),
    recording_location  = first(na.omit(recording_location)),
    .groups = "drop"
  ) %>%
  mutate(
    sampling_rate_min = ifelse(is.infinite(sampling_rate_min), NA_real_, sampling_rate_min),
    sampling_rate_max = ifelse(is.infinite(sampling_rate_max), NA_real_, sampling_rate_max)
  ) %>%
  pivot_wider(
    id_cols = device_id,
    names_from = signal_key,
    values_from = c(available, sampling_rate_min, sampling_rate_max, additional_info, recording_location),
    names_glue = "{signal_key}_{.value}"
  )

# * 7 data acces (long -> wide) ----
df_shiny_data_access <- data_access %>%
  mutate(
    spec_key       = norm_key(spec_name),
    spec_num_value = suppressWarnings(as.numeric(spec_num_value))
  ) %>%
  group_by(device_id, spec_key) %>%
  summarise(
    spec_boel_value = first(na.omit(spec_boel_value), default = NA),
    spec_num_value  = suppressWarnings(max(spec_num_value, na.rm = TRUE)),
    spec_num_unit   = first(na.omit(spec_num_unit),  default = NA),
    spec_char_value = first(na.omit(spec_char_value), default = NA),
    .groups = "drop"
  ) %>%
  mutate(
    spec_num_value = ifelse(is.infinite(spec_num_value), NA_real_, spec_num_value)
  ) %>%
  pivot_wider(
    id_cols = device_id,
    names_from = spec_key,
    values_from = c(spec_boel_value, spec_num_value, spec_num_unit, spec_char_value),
    names_glue = "{spec_key}_{.value}"
  )

# * 8 rvu df (long -> wide) ----
df_shiny_rvu <- rvu %>%
  mutate(
    synth_key     = norm_key(synthesis_type),
    n_of_studies  = suppressWarnings(as.integer(n_of_studies))
  ) %>%
  group_by(device_id, synth_key) %>%
  summarise(
    n_of_studies         = suppressWarnings(max(n_of_studies, na.rm = TRUE)),
    evidence_level       = first(na.omit(evidence_level),       default = NA),
    parameters_studied   = first(na.omit(parameters_studied),   default = NA),
    synthesis            = first(na.omit(synthesis),            default = NA),
    date_of_last_search  = first(na.omit(date_of_last_search),  default = NA),
    .groups = "drop"
  ) %>%
  mutate(
    n_of_studies = ifelse(is.infinite(n_of_studies), NA_integer_, n_of_studies)
  ) %>%
  pivot_wider(
    id_cols = device_id,
    names_from = synth_key,
    values_from = c(n_of_studies, evidence_level, parameters_studied, synthesis, date_of_last_search),
    names_glue = "{synth_key}_{.value}"
  )

# * 9 create final shiny df ----
df_shiny_wi <- df_shiny_scores %>%
  left_join(df_shiny_devices,   by = "device_id") %>%
  left_join(df_shiny_specs,     by = "device_id") %>%
  left_join(df_shiny_signals,   by = "device_id") %>%
  left_join(df_shiny_data_access, by = "device_id") %>%
  left_join(df_shiny_rvu,       by = "device_id")

# * 10 write final shiny df ----
saveRDS(df_shiny_wi, here("data", "processed", "df_shiny_wi.rds"))
saveRDS(df_shiny_wi, here("output","data", "df_shiny_wi.rds"))
write_csv(df_shiny_wi, here("output","data", "df_shiny_wi.csv"))
write_xlsx(list(df_shiny_wi = df_shiny_wi), here("output","data", "df_shiny_wi.xlsx"))

