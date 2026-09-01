#' Penalized Longitudinal CFA with Alignment-Style Penalties
#'
#' A wrapper function that performs penalized estimation for longitudinal CFA
#' models by applying penalties to differences in loadings, intercepts, and
#' residual variances across time points. This function internally generates
#' a configural invariance model with free latent means and variances, then
#' applies penalized estimation.
#'
#' @param ind_matrix A character matrix where each column represents a time
#'   point and each row represents an indicator variable. Column names should
#'   be time point labels, and cell values should be variable names in the data.
#' @param lv_names Character vector of latent variable names for each time point.
#'   If NULL, default names will be generated.
#' @param data A data frame containing the observed variables. Optional if
#'   `sample.cov` is provided.
#' @param sample.cov A numeric covariance matrix. Optional if `data` is provided.
#' @param sample.mean A numeric vector of means. Optional if `data` is provided.
#' @param sample.nobs Numeric scalar. The number of observations. Required if
#'   using summary statistics instead of raw data.
#' @param w Numeric scalar. Penalty weight applied to the penalty terms.
#'   Default is 0.1.
#' @param pen_fn Character string specifying the penalty function. Options are
#'   `"l0a"` (default) or `"alf"`.
#' @param pen_params Character vector specifying which parameter types to
#'   penalize. Options are `"loadings"`, `"intercepts"`, and `"residuals"`.
#'   Default is `c("loadings", "intercepts")`.
#' @param se Character string specifying the type of standard errors. Default
#'   is `"none"`. See [plavaan::penalized_est()] for options.
#' @param test Character string specifying the model test to compute. Options
#'   are `"none"` (default), `"Chisq"`, and `"SatorraBentler"`. Fit measures
#'   ([lavaan::fitmeasures()]) and the chi-square test in [summary()] are only
#'   available when `test` is not `"none"`; they are evaluated at the effective
#'   degrees of freedom (see [plavaan::effective_df()]). Fit evaluation is
#'   experimental. See [plavaan::penalized_est()] for details.
#' @param opt_control A list of control parameters passed to [stats::nlminb()].
#'   See [plavaan::penalized_est()] for defaults.
#' @param ... Additional arguments passed to [longcfa()].
#'
#' @return A lavaan model object with penalized parameter estimates. See
#'   [plavaan::penalized_est()] for details on interpretation.
#'
#' @details
#' This function simplifies the workflow for penalized longitudinal CFA by:
#' \enumerate{
#'   \item Generating a configural invariance model syntax with free latent
#'         means and variances
#'   \item Identifying the relevant parameter IDs for loadings, intercepts,
#'         and residuals
#'   \item Applying [plavaan::penalized_est()] with penalties on pairwise differences
#' }
#'
#' The penalty is applied to differences between corresponding parameters at
#' different time points, encouraging approximate measurement invariance.
#'
#' **Fit measures:** Setting `test` to `"Chisq"` or `"SatorraBentler"` enables
#' [lavaan::fitmeasures()] and the chi-square test in [summary()], which are
#' computed at the effective degrees of freedom. This relies on the
#' experimental fit-evaluation support in `plavaan`.
#'
#' **Note:** If using summary statistics (`sample.cov`, `sample.mean`, `sample.nobs`),
#' ordered/categorical items cannot be automatically handled because threshold
#' counts must be derived from raw data.
#'
#' @seealso [plavaan::penalized_est()], [longcfa()], [longcfa_syntax()]
#'
#' @export
#' @examples
#' \dontrun{
#' library(lavaan)
#'
#' # Prepare indicator matrix
#' ind_mat <- cbind(c("y1", "y2", "y3", "y4"), c("y5", "y6", "y7", "y8"))
#'
#' # Fit penalized longitudinal CFA with raw data
#' pen_fit <- penalized_longcfa(
#'     ind_matrix = ind_mat,
#'     lv_names = c("dem60", "dem65"),
#'     data = PoliticalDemocracy,
#'     w = 0.1,
#'     pen_fn = "alf"
#' )
#'
#' # Fit measures are available when a model test is enabled (experimental)
#' pen_fit_test <- penalized_longcfa(
#'     ind_matrix = ind_mat,
#'     lv_names = c("dem60", "dem65"),
#'     data = PoliticalDemocracy,
#'     w = 0.1,
#'     test = "Chisq"
#' )
#' lavaan::fitmeasures(pen_fit_test, c("chisq", "df", "cfi", "rmsea", "srmr"))
#'
#' # Fit penalized longitudinal CFA with summary statistics
#' pen_fit_stat <- penalized_longcfa(
#'     ind_matrix = ind_mat,
#'     lv_names = c("dem60", "dem65"),
#'     sample.cov = my_cov,
#'     sample.mean = my_means,
#'     sample.nobs = 500,
#'     w = 0.1,
#'     pen_fn = "alf"
#' )
#'
#' # Compare with scalar invariance model
#' fit_scalar <- longcfa(
#'     ind_mat,
#'     lv_names = c("dem60", "dem65"),
#'     data = PoliticalDemocracy,
#'     long_equal = c("loadings", "intercepts")
#' )
#'
#' cbind(
#'     penalized = coef(pen_fit),
#'     scalar = coef(fit_scalar)
#' )
#' }
penalized_longcfa <- function(
    ind_matrix,
    lv_names = NULL,
    data = NULL,
    sample.cov = NULL,
    sample.mean = NULL,
    sample.nobs = NULL,
    w = 0.1,
    pen_fn = "l0a",
    pen_params = c("loadings", "intercepts"),
    se = "none",
    test = "none",
    opt_control = list(),
    ...
) {
    # Validate pen_params
    valid_params <- c(
        "loadings",
        "intercepts",
        "thresholds",
        "residuals"
    )
    if (!all(pen_params %in% valid_params)) {
        stop(
            "pen_params must contain only 'loadings', 'intercepts', 'thresholds', ",
            "and/or 'residuals'"
        )
    }

    if (is.null(data) && is.null(sample.cov)) {
        stop("Either `data` or `sample.cov` must be provided.")
    }

    # Create unfitted model object for penalized estimation
    fit_unfitted <- longcfa(
        ind_matrix,
        lv_names = lv_names,
        data = data,
        sample.cov = sample.cov,
        sample.mean = sample.mean,
        sample.nobs = sample.nobs,
        free_latvars = TRUE,
        free_latmeans = TRUE,
        do.fit = FALSE,
        ...
    )

    # Build penalty difference ID list
    pen_diff_id <- list()

    # Cache parameter table — fetched once instead of once per parameter type
    pt_cached <- lavaan::partable(fit_unfitted)

    if ("loadings" %in% pen_params) {
        load_ids <- par_to_mat_from_pt(
            pt_cached,
            op = "=~",
            ind_matrix = ind_matrix,
            out_col = "free"
        )
        pen_diff_id$loadings <- t(load_ids)
    }

    if ("intercepts" %in% pen_params) {
        int_ids <- par_to_mat_from_pt(
            pt_cached,
            op = "~1",
            ind_matrix = ind_matrix,
            out_col = "free"
        )
        pen_diff_id$intercepts <- t(int_ids)
    }

    if ("thresholds" %in% pen_params) {
        thresh_ids <- par_to_mat_from_pt(
            pt_cached,
            op = "|",
            ind_matrix = ind_matrix,
            out_col = "free"
        )
        pen_diff_id$thresholds <- t(thresh_ids)
    }

    if ("residuals" %in% pen_params) {
        pt_resid <- pt_cached[
            pt_cached$op == "~~" & pt_cached$lhs == pt_cached$rhs,
            ,
            drop = FALSE
        ]
        idx <- match(ind_matrix, pt_resid$lhs)
        resid_ids <- matrix(
            pt_resid$free[idx],
            nrow = nrow(ind_matrix),
            ncol = ncol(ind_matrix)
        )
        pen_diff_id$residuals <- t(resid_ids)
    }

    # Check that at least one parameter type is selected
    if (length(pen_diff_id) == 0) {
        stop("pen_params must contain at least one valid parameter type")
    }

    # Apply penalized estimation
    plavaan::penalized_est(
        x = fit_unfitted,
        w = w,
        pen_diff_id = pen_diff_id,
        pen_fn = pen_fn,
        se = se,
        test = test,
        opt_control = opt_control
    )
}
