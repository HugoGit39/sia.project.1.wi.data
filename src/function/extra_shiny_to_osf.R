########################################################################
# Helpers for human-readable sheet
#
# yn_to_flag():
#   Convert various Yes/No-like values (e.g. "Yes", "no", "1", "0")
#   into integer flags:
#     - 1 for yes/true-like values
#     - 0 for no/false-like values
#     - NA for everything else / missing
#
# rate_str():
#   Combine sampling_rate_min and sampling_rate_max into a single
#   character string:
#     - both NA          -> NA
#     - only one present -> that value as text
#     - equal            -> single value ("64")
#     - different        -> "min-max" (e.g. "26-208")
#
# paste_nonempty():
#   Row-wise, concatenate multiple values into a single string:
#     - drops NA and "" values
#     - joins remaining parts with "; "
#     - returns NA if all parts are empty
#   Used to build compact cells like "1; reflection; 64-208; wrist".
#
# Stress in Action 2025
########################################################################

yn_to_flag <- function(x) {
  x <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    x %in% c("yes","y","true","t","1") ~ 1L,
    x %in% c("no","n","false","f","0") ~ 0L,
    TRUE ~ NA_integer_
  )
}

rate_str <- function(minv, maxv) {
  dplyr::case_when(
    is.na(minv) & is.na(maxv) ~ NA_character_,
    is.na(minv)               ~ as.character(maxv),
    is.na(maxv)               ~ as.character(minv),
    minv == maxv              ~ as.character(minv),
    TRUE                      ~ paste0(minv, "-", maxv)
  )
}

paste_nonempty <- function(...) {
  args <- list(...)
  n <- max(vapply(args, length, integer(1L)))
  res <- rep(NA_character_, n)
  
  for (i in seq_len(n)) {
    vals <- vapply(args, function(a) {
      if (length(a) < i) return(NA_character_)
      v <- as.character(a[i])
      if (is.na(v) || v == "") NA_character_ else v
    }, character(1))
    vals <- vals[!is.na(vals) & vals != ""]
    if (length(vals) > 0) res[i] <- paste(vals, collapse = "; ")
  }
  res
}