#' Sample Ecological Momentary Assessment Data
#'
#' Daily diary data from an ecological momentary assessment study examining
#' state social anxiety and perfectionistic self-presentation across 20 days.
#'
#' @format A data frame with 4,252 rows and 12 columns (long format):
#' \describe{
#'   \item{id}{Participant identifier}
#'   \item{day}{Day of measurement (1-8)}
#'   \item{ssa1}{State social anxiety item 1}
#'   \item{ssa2}{State social anxiety item 2}
#'   \item{ssa3}{State social anxiety item 3}
#'   \item{ssa4}{State social anxiety item 4}
#'   \item{ssa5}{State social anxiety item 5}
#'   \item{ssa6}{State social anxiety item 6}
#'   \item{ssa7}{State social anxiety item 7}'
#'   \item{psp1}{Perfectionistic self-presentation item 1}
#'   \item{psp2}{Perfectionistic self-presentation item 2}
#'   \item{psp3}{Perfectionistic self-presentation item 3}
#' }
#'
#' @source Data available at: \url{https://osf.io/hwkem/files/r2qx4},
#' @references Mackinnon, S., Curtis, R., & O'Connor, R. (2022). A tutorial in longitudinal measurement invariance and cross-lagged panel models using lavaan. Meta-Psychology, 6. \url{https://doi.org/10.15626/MP.2020.2595}
#'
#'   Project page: \url{https://osf.io/hwkem/}, \url{https://osf.io/gduy4/}
"mackinnon_etal_long"

#' @rdname mackinnon_etal_long
#' @format The wide format data only covers days 2-8, with 261 rows and 71 columns, and the days are
#' indicated by suffixes (e.g., `ssa1_2` for day 2).
"mackinnon_etal_wide"
