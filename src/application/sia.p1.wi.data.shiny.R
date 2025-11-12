# ---- packages ----
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(writexl)
library(readr)
library(magrittr)

# ---- helpers ----
yn_to_logical <- function(x) {
  d <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    d %in% c("yes","y","true","t","1") ~ TRUE,
    d %in% c("no","n","false","f","0") ~ FALSE,
    TRUE ~ NA
  )
}

norm_id <- function(x) {
  x %>% as.character() %>% trimws()
}

# Return the first present column from `candidates`; otherwise a vector of NA of length nrow(df)
get_col <- function(df, candidates, default = NA) {
  nm <- intersect(candidates, names(df))[1]
  if (length(nm) == 0 || is.na(nm)) {
    rep(default, nrow(df))
  } else {
    df[[nm]]
  }
}

# ---- input paths (Excel) ----
p_devices     <- here("data","raw","devices.xlsx")
p_signals     <- here("data","raw","signals.xlsx")
p_specs       <- here("data","raw","technical_specs.xlsx")
p_data_access <- here("data","raw","data_access.xlsx")
p_vru         <- here("data","raw","rvu_synthesis.xlsx")
p_scores      <- here("data","raw","expert_scores.xlsx")

# ---- DEVICES ----
devices_raw <- read_xlsx(p_devices, sheet = 1) %>%
  clean_names()

devices <- devices_raw %>%
  mutate(
    device_id = norm_id(get_col(., c("device_id","id","device")))
  ) %>%
  filter(!is.na(device_id) & nzchar(device_id)) %>%
  distinct(device_id, .keep_all = TRUE)

# ---- SIGNALS (long -> wide) ----
signals_raw <- read_xlsx(p_signals, sheet = 1) %>%
  clean_names()

signals_long <- signals_raw %>%
  mutate(
    device_id        = norm_id(get_col(., c("device_id","id","device"))),
    signal_name      = get_col(., c("signal_name","signal","name")) %>% tolower() %>% make_clean_names(),
    available        = yn_to_logical(get_col(., c("available","availability","available_flag","present","has_signal"))),
    sampling_rate_hz = suppressWarnings(as.numeric(
      get_col(., c("sampling_rate_hz","sampling_rate","hz","frequency_hz","freq_hz","rate_hz"))
    )),
    units            = get_col(., c("units","unit"))
  ) %>%
  select(device_id, signal_name, available, sampling_rate_hz, units) %>%
  filter(!is.na(device_id) & nzchar(device_id) & !is.na(signal_name) & nzchar(signal_name))

signals_wide <- signals_long %>%
  group_by(device_id, signal_name) %>%
  summarise(
    available = any(available %in% TRUE, na.rm = TRUE),
    sampling_rate_hz = suppressWarnings(max(sampling_rate_hz, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    sampling_rate_hz = ifelse(is.infinite(sampling_rate_hz), NA_real_, sampling_rate_hz)
  ) %>%
  pivot_wider(
    id_cols = device_id,
    names_from = signal_name,
    values_from = c(available, sampling_rate_hz),
    names_glue = "{signal_name}_{.value}"
  )

# ---- TECHNICAL SPECS (long -> wide) ----
specs_raw <- read_xlsx(p_specs, sheet = 1) %>%
  clean_names()

specs_long <- specs_raw %>%
  mutate(
    device_id = norm_id(get_col(., c("device_id","id","device"))),
    spec_name = get_col(., c("spec_name","name","spec")) %>% tolower() %>% make_clean_names(),
    spec_value = get_col(., c("spec_value","value","val"))
  ) %>%
  select(device_id, spec_name, spec_value) %>%
  filter(!is.na(device_id) & nzchar(device_id) & !is.na(spec_name) & nzchar(spec_name))

specs_wide <- specs_long %>%
  group_by(device_id, spec_name) %>%
  summarise(spec_value = dplyr::first(na.omit(spec_value)), .groups = "drop") %>%
  pivot_wider(
    id_cols = device_id,
    names_from = spec_name,
    values_from = spec_value,
    names_glue = "spec_{spec_name}"
  )

# ---- DATA ACCESS (one row per device) ----
data_access_raw <- read_xlsx(p_data_access, sheet = 1) %>%
  clean_names()

data_access <- data_access_raw %>%
  mutate(
    device_id       = norm_id(get_col(., c("device_id","id","device"))),
    data_storage    = get_col(., c("data_storage","storage","store")),
    raw_data_access = yn_to_logical(get_col(., c("raw_data_access","raw_data","raw","raw_access"))),
    sdk_available   = yn_to_logical(get_col(., c("sdk_available","sdk","sdk_yn"))),
    api_available   = yn_to_logical(get_col(., c("api_available","api","api_yn")))
  ) %>%
  select(device_id, data_storage, raw_data_access, sdk_available, api_available) %>%
  filter(!is.na(device_id) & nzchar(device_id)) %>%
  distinct(device_id, .keep_all = TRUE)

# ---- VRU (Validity / Reliability / Usability) ----
vru_raw <- read_xlsx(p_vru, sheet = 1) %>%
  clean_names()

vru <- vru_raw %>%
  mutate(
    device_id              = norm_id(get_col(., c("device_id","id","device"))),
    validation_level       = get_col(., c("validation_level","validation","level")),
    num_studies            = suppressWarnings(as.integer(get_col(., c("num_studies","studies","n_studies")))),
    studied_parameters     = get_col(., c("studied_parameters","parameters","params")),
    synthesis_validity     = get_col(., c("synthesis_validity","validity_synthesis","validity_summary")),
    num_usability_studies  = suppressWarnings(as.integer(get_col(., c("num_usability_studies","usability_studies","n_usability")))),
    synthesis_usability    = get_col(., c("synthesis_usability","usability_synthesis","usability_summary"))
  ) %>%
  select(device_id, validation_level, num_studies, studied_parameters,
         synthesis_validity, num_usability_studies, synthesis_usability) %>%
  filter(!is.na(device_id) & nzchar(device_id)) %>%
  distinct(device_id, .keep_all = TRUE)

# ---- EXPERT SCORES (long -> wide) ----
scores_raw <- read_xlsx(p_scores, sheet = 1) %>%
  clean_names()

scores_long <- scores_raw %>%
  mutate(
    device_id  = norm_id(get_col(., c("device_id","id","device"))),
    score_type = get_col(., c("score_type","type","category")) %>% tolower() %>% make_clean_names(),
    score      = suppressWarnings(as.numeric(get_col(., c("score","value","val"))))
  ) %>%
  select(device_id, score_type, score) %>%
  filter(!is.na(device_id) & nzchar(device_id) & !is.na(score_type) & nzchar(score_type))

scores_wide <- scores_long %>%
  group_by(device_id, score_type) %>%
  summarise(score = mean(score, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = device_id,
    names_from = score_type,
    values_from = score,
    names_glue = "score_{score_type}"
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

