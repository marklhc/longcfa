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
#' @param opt_control A list of control parameters passed to [stats::nlminb()].
#'   See [plavaan::penalized_est()] for defaults.
#' @param test Character string specifying the model test to compute. Options
#'   are `"none"` (default), `"Chisq"`, and `"SatorraBentler"`. Fit measures
#'   ([lavaan::fitmeasures()]) and the chi-square test in [summary()] are only
#'   available when `test` is not `"none"`; they are evaluated at the effective
#'   degrees of freedom (see `plavaan::effective_df()`). Fit evaluation is
#'   experimental and requires a `plavaan` build with fit-evaluation support;
#'   on older builds a non-`"none"` `test` will error. See
#'   [plavaan::penalized_est()] for details.
#' @param plavaan_args A named list of additional arguments forwarded to the
#'   underlying `plavaan` estimator ([plavaan::penalized_est()], or
#'   [plavaan::penalized_est_multistart()] when multistart is requested). This is
#'   an escape hatch for options not exposed as dedicated arguments, for example:
#'   \itemize{
#'     \item `eps` / `telescoping_control` — smoothing and continuation control
#'           for the built-in penalties (e.g. `eps = "telescoping"`).
#'     \item `n_starts`, `starts`, `keep_all`, `verbose` — multistart control.
#'           Supplying `starts` (or `n_starts > 1`) switches to
#'           [plavaan::penalized_est_multistart()].
#'     \item `start` — custom starting values for a single-start fit.
#'   }
#'   Only arguments accepted by the installed `plavaan` build are forwarded; an
#'   option the build does not support produces an error suggesting an update.
#'   Options that have a dedicated argument (`w`, `pen_fn`, `se`, `opt_control`,
#'   `test`) must be set via that argument, not through `plavaan_args`.
#'   See [plavaan::penalized_est()] and [plavaan::penalized_est_multistart()]
#'   for the full set of options.
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
#' **Multistart and penalty continuation:** Non-convex penalties (`l0a`, `alf`)
#' can have local optima. Set `plavaan_args = list(n_starts = k)` (or supply
#' `starts`) to run [plavaan::penalized_est_multistart()] and keep the best
#' solution, or `plavaan_args = list(eps = "telescoping")` to fit a continuation
#' sequence of decreasing penalty smoothing. Fit measures (`test`) are not
#' available together with multistart.
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
#' # Penalty continuation ("telescoping") and multistart via plavaan_args
#' pen_fit_tele <- penalized_longcfa(
#'     ind_matrix = ind_mat,
#'     lv_names = c("dem60", "dem65"),
#'     data = PoliticalDemocracy,
#'     w = 0.1,
#'     plavaan_args = list(eps = "telescoping")
#' )
#' set.seed(1)
#' pen_fit_ms <- penalized_longcfa(
#'     ind_matrix = ind_mat,
#'     lv_names = c("dem60", "dem65"),
#'     data = PoliticalDemocracy,
#'     w = 0.1,
#'     plavaan_args = list(n_starts = 10)
#' )
#' attr(pen_fit_ms, "multistart")
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
    opt_control = list(),
    test = "none",
    plavaan_args = list(),
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

    # `plavaan_args` must be a (possibly empty) named plain list with non-empty
    # names (no NA or ""), so that `$` access and forwarding are well-defined.
    # Data frames (and tibbles) are lists to is.list() and are rejected so the
    # type check matches the documented "named list" contract.
    if (is.data.frame(plavaan_args) || !is.list(plavaan_args)) {
        stop("`plavaan_args` must be a named list.")
    }
    pa_names <- names(plavaan_args)
    if (
        length(plavaan_args) > 0 &&
            (is.null(pa_names) || any(is.na(pa_names)) || any(!nzchar(pa_names)))
    ) {
        stop("`plavaan_args` must be a named list.")
    }

    # Multistart is requested when `starts` is supplied or `n_starts > 1` (matching
    # the docs). A missing/NA `n_starts`, or one that is `<= 1`, means a single
    # start; in that case `n_starts`/`starts` are dropped so they are not forwarded
    # to penalized_est(), which does not accept them. The value-based check also
    # keeps `use_multistart` a plain TRUE/FALSE (an NA `n_starts` cannot leak an NA
    # into the `if (use_multistart && ...)` guard below).
    n_starts <- plavaan_args$n_starts
    use_multistart <-
        !is.null(plavaan_args$starts) ||
        (is.numeric(n_starts) && length(n_starts) == 1 && !is.na(n_starts) &&
            n_starts > 1)
    est_plavaan_args <- if (use_multistart) {
        plavaan_args
    } else {
        plavaan_args[setdiff(pa_names, c("n_starts", "starts"))]
    }

    # penalized_longcfa() needs plavaan (a Suggested package, so the package
    # loads without it). Fail fast with a clear message if it is missing,
    # distinct from the "installed but too old" case below.
    if (!requireNamespace("plavaan", quietly = TRUE)) {
        stop(
            "penalized_longcfa() requires the plavaan package, which is not ",
            "installed. Please install plavaan."
        )
    }

    # Multistart needs penalized_est_multistart() (plavaan >= 0.0.2); a build
    # without it fails with a clear message rather than a later resolution error.
    if (
        use_multistart &&
            !("penalized_est_multistart" %in% getNamespaceExports("plavaan"))
    ) {
        stop(
            "Multistart (n_starts > 1 or starts) requires a plavaan build that ",
            "exports penalized_est_multistart() (plavaan >= 0.0.2). Please ",
            "update plavaan."
        )
    }

    # Validate the requested test (type, length, NA, then membership) so that
    # NULL / NA / length > 1 inputs give a clean message instead of a raw
    # `if()` error.
    if (
        !is.character(test) || length(test) != 1L || is.na(test) ||
            !test %in% c("none", "Chisq", "SatorraBentler")
    ) {
        stop("`test` must be a single string: 'none', 'Chisq', or 'SatorraBentler'.")
    }

    # Fit measures (test) are not available on penalized_est_multistart() in any
    # released plavaan. Suggest a single start only when fit evaluation is
    # actually available (i.e. penalized_est() supports `test`); otherwise fall
    # through so the `missing` check below reports that this build lacks fit
    # evaluation entirely (single start would fail too).
    if (
        use_multistart && !identical(test, "none") &&
            ("test" %in% names(formals(plavaan::penalized_est))) &&
            !("test" %in% names(formals(plavaan::penalized_est_multistart)))
    ) {
        stop(
            "`test` (fit measures) is not available with multistart in this ",
            "plavaan build. Use a single start to obtain fit measures."
        )
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

    # ---- Robust dispatch to the installed plavaan estimator ----
    # Only forward arguments the installed plavaan actually accepts. Defaults
    # (e.g. test = "none" on an older build) are dropped silently; arguments the
    # user explicitly supplied require support, so a missing feature yields a
    # clear "update plavaan" message rather than an `unused argument` error.
    target_fn <-
        if (use_multistart) plavaan::penalized_est_multistart
        else plavaan::penalized_est

    # `test` is always included so it is forwarded whenever the target estimator
    # accepts it (including a future penalized_est_multistart() that gains `test`).
    base_args <- list(
        x = fit_unfitted,
        w = w,
        pen_diff_id = pen_diff_id,
        pen_fn = pen_fn,
        se = se,
        opt_control = opt_control,
        test = test
    )

    # Options in base_args are not settable via plavaan_args. The user-facing
    # ones have dedicated arguments; the internal inputs (x, pen_diff_id) are
    # reserved. Reject both so (e.g.) `test` cannot be silently overridden or
    # bypass the validation and multistart checks above, with an accurate message
    # for each category.
    dedicated <- c("w", "pen_fn", "se", "opt_control", "test")
    reserved <- setdiff(names(base_args), dedicated)
    pa_dedicated <- intersect(names(plavaan_args), dedicated)
    if (length(pa_dedicated)) {
        stop(
            "The following are dedicated arguments of penalized_longcfa(), not ",
            "plavaan_args options: ", paste(pa_dedicated, collapse = ", "),
            ". Set them directly, not via plavaan_args."
        )
    }
    pa_reserved <- intersect(names(plavaan_args), reserved)
    if (length(pa_reserved)) {
        stop(
            "The following are reserved for internal use and cannot be set via ",
            "plavaan_args: ", paste(pa_reserved, collapse = ", ")
        )
    }

    # est_plavaan_args holds the estimator options not controlled by dedicated
    # args (e.g. eps/telescoping_control, start, n_starts/starts for multistart).
    # mergeList (not c()) so any residual overlap overwrites instead of
    # duplicating a name, which would break do.call().
    all_args <- utils::modifyList(base_args, est_plavaan_args)

    supported <- names(formals(target_fn))
    required <- c(
        names(est_plavaan_args),
        if (!identical(test, "none")) "test"
    )
    missing <- setdiff(required, supported)
    if (length(missing)) {
        stop(
            "This plavaan build (", as.character(utils::packageVersion("plavaan")),
            ") does not support: ", paste(missing, collapse = ", "),
            ". Please update plavaan, or omit the unsupported option(s)."
        )
    }

    do.call(target_fn, all_args[names(all_args) %in% supported])
}
