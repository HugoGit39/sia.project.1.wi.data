# * 1 packages ----
library(here)       # file paths: here("data","raw","devices.xlsx")
library(readxl)     # read_xlsx()
library(dplyr)      # %>%, mutate, group_by, summarise, transmute, left_join, arrange, coalesce
library(tidyr)      # pivot_wider()
library(janitor)    # clean_names()
library(writexl)    # write_xlsx()
library(readr)      # write_csv()  (used for df_shiny_wi export)