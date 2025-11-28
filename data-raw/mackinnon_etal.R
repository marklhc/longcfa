## code to prepare `mackinnon_etal` dataset goes here
# OSF Link: https://osf.io/hwkem/files/r2qx4
mackinnon_etal <- foreign::read.spss(
  "https://osf.io/download/r2qx4/",
  to.data.frame = TRUE
)

# Create long format dataset with selected variables
ssa_vars <- grep("^ssa[1-7]", names(mackinnon_etal), value = TRUE)
psp_vars <- grep("^psp[1-3]", names(mackinnon_etal), value = TRUE)
mackinnon_etal_long <- mackinnon_etal[c("id", "day", ssa_vars, psp_vars)] |>
  as.data.frame()

usethis::use_data(mackinnon_etal_long, overwrite = TRUE)

# Create wide format dataset
mackinnon_etal_wide <- mackinnon_etal_long |>
  # Select only days 2-8
  dplyr::filter(day %in% 2:8) |>
  # Convert day to factor
  dplyr::mutate(day = as.factor(day)) |>
  tidyr::pivot_wider(
    id_cols = id,
    names_from = day,
    values_from = ssa1:psp3,
    names_sep = "_",
    names_sort = TRUE
  ) |>
  as.data.frame()

usethis::use_data(mackinnon_etal_wide, overwrite = TRUE)
