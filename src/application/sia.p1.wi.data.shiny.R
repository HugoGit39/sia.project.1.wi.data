

# * 1 packages ----
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(writexl)
library(readr)
library(magrittr)

# * 2  functions ----
here("src","function","norm_key.R")

# * 3 read-in data ----
p_devices     <- here("data","raw","devices.xlsx")
p_signals     <- here("data","raw","signals.xlsx")
p_specs       <- here("data","raw","technical_specs.xlsx")
p_data_access <- here("data","raw","data_access.xlsx")
p_rvu         <- here("data","raw","rvu_synthesis.xlsx")
p_scores      <- here("data","raw","expert_scores.xlsx")

devices <- read_xlsx(p_devices) %>% clean_names()
signals_long <- read_xlsx(p_signals) %>% clean_names()
specs <- read_xlsx(p_specs) %>% clean_names()
data_access <- read_xlsx(p_data_access) %>% clean_names()
rvu <- read_xlsx(p_rvu) %>% clean_names()
scores <- read_xlsx(p_scores) %>% clean_names()

# * 4 devices df ----
devices_raw <- read_xlsx(p_devices) 

# * 5 signals df (long -> wide) ----
signals_wide <- signals_long %>%
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

# * 6 technical specs (long -> wide) ----
specs_wide <- specs %>%
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

# * 7 data acces (long -> wide) ----
data_access_wide <- data_access %>%
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
rvu_wide <- rvu %>%
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

# * 9 expert scores (long -> wide) ----
scores_wide <- scores %>%
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

# ---- assemble the shiny-wide df (one row per device) ----
df_shiny_wi <- devices %>%
  left_join(signals_wide,   by = "device_id") %>%
  left_join(specs_wide,     by = "device_id") %>%
  left_join(data_access,    by = "device_id") %>%
  left_join(vru,            by = "device_id") %>%
  left_join(scores_wide,    by = "device_id") %>%
  relocate(device_id, .before = 1) %>%
  arrange(device_id)

# ---- output (ensure folder, then write CSV + Excel) ----

readr::write_csv(df_shiny_wi, here("output","data"))
writexl::write_xlsx(list(df_shiny_wi = df_shiny_wi), here("output","data"))

