########################################################################
# norm_key() function
#
# Purpose:
#   Convert free-text labels (e.g., spec_name, signal_name) into stable,
#   machine-friendly column keys WITHOUT forcing global uniqueness.
#
# What it does:
#   - lowercase
#   - trim whitespace
#   - replace non-alphanumeric with "_"
#   - collapse repeated underscores
#   - trim leading/trailing underscores
#
# Stress in Action 2025
#####################################################################
norm_key <- function(x) {
  x %>%
    tolower() %>%
    trimws() %>%
    gsub("[^a-z0-9]+", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .)
}
