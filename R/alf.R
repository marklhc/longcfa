# Write a function that computes the penalized log-likelihood of a growth model
# Write a function that computes the gradient (gradient of LL - gradient of penalty)
# Find out how to obtain LL from lavaan
# Obtain analytic gradient from lavaan
# Write function for optimization using optim . . .

#' Composite Pairwise Loss Function
#'
#' Computes the total loss across all pairwise combinations of rows in a matrix.
#'
#' @param x A numeric vector, matrix, or data frame. If not a matrix, it will be
#'   coerced to one after applying the transformation function.
#' @param fun A function to compute the loss for each pairwise difference.
#'   The package supports the alignment loss (`alf`) and the approximate L0 penalty
#'   (`l0a`), but users can provide custom functions as well.
#' @param trans A transformation function to apply to `x` before computing
#'   pairwise differences. Default is `identity` (no transformation).
#' @param rescale Either `"df"` (default) to rescale the total loss by the degrees
#'   of freedom (number of rows - 1), or a numeric value (likely between 0 and 1)
#'   to multiply the total loss by.
#' @param ... Additional arguments passed to the loss function `fun`.
#'
#' @return A numeric scalar representing the sum of losses across all pairwise
#'   combinations of rows.
#'
#' @details
#' The function works by:
#' \enumerate{
#'   \item Applying the transformation function `trans` to the input `x`
#'   \item Converting the result to a matrix
#'   \item Generating all possible pairwise combinations of row indices
#'   \item Computing the difference between each pair of rows
#'   \item Applying the loss function `fun` to each difference
#'   \item Summing all the individual losses
#' }
#'
#' @examples
#' # Example with a simple matrix
#' x <- matrix(runif(12), nrow = 4)
#' composite_pair_loss(x, fun = alf)
#'
#' # Example with log transformation and L2 loss
#' composite_pair_loss(x, fun = function(x) x^2, trans = log)
#'
#' @importFrom utils combn
#' @export
composite_pair_loss <- function(x, fun, trans = identity, rescale = "df", ...) {
    x <- as.matrix(trans(x))
    combn_idx <- combn(nrow(x), 2)
    if (rescale == "df") {
        dof <- nrow(x) - 1
        ncombn <- ncol(combn_idx)
        rescale <- dof / ncombn
    }
    if (!is.numeric(rescale)) {
        stop("rescale must be 'df' or a numeric value.")
    }
    out <- fun(x[combn_idx[1, ], ] - x[combn_idx[2, ], ], ...)
    sum(out) * rescale
}

hot_gr <- function(x, hot, fun, ...) {
    out <- 0 * x
    if (is.matrix(hot)) {
        x_hot <- matrix(x[hot], nrow = nrow(hot), ncol = ncol(hot))
    } else {
        x_hot <- x[hot]
    }
    out[hot] <- fun(x_hot, ...)
    out
}

gr_cpl <- function(
    x,
    gr_fun,
    trans = identity,
    gr_trans = function(x) 1,
    rescale = "df"
) {
    x_mat <- as.matrix(trans(x))
    combn_idx <- combn(nrow(x_mat), 2)
    diffs <- x_mat[combn_idx[1, ], , drop = FALSE] -
        x_mat[combn_idx[2, ], , drop = FALSE]
    grad_contribs <- gr_fun(diffs)
    grad <- matrix(0, nrow = nrow(x_mat), ncol = ncol(x_mat))
    for (i in seq_len(nrow(x_mat))) {
        idx1 <- which(combn_idx[1, ] == i)
        idx2 <- which(combn_idx[2, ] == i)
        grad[i, ] <- colSums(grad_contribs[idx1, , drop = FALSE]) -
            colSums(grad_contribs[idx2, , drop = FALSE])
    }
    if (rescale == "df") {
        dof <- nrow(x_mat) - 1
        ncombn <- ncol(combn_idx)
        rescale <- dof / ncombn
    }
    if (!is.numeric(rescale)) {
        stop("rescale must be 'df' or a numeric value.")
    }
    as.vector(grad) * rescale * gr_trans(as.vector(x))
}

#' Loss functions
#'
#' For small eps this provides a smooth,
#' numerically stable approximation of |x|^(1/2) (i.e. the square root of
#' the absolute value). The function is vectorized over x.
#'
#' @param x Numeric vector. Input values to transform.
#' @param eps Positive numeric scalar (default .001 for `alf()` and .01
#'   for `l0a()`). Small regularization constant to avoi
#'   non-differentiability and division-by-zero issues.
#' @return Numeric vector of the same length as x.
#' @name loss
NULL
#> NULL

#' @rdname loss
#'
#' @details The ALF, (x^2 + eps)^(1/4), is useful when a smooth
#'   surrogate for sqrt(|x|) is required (for optimization or
#'   regularization) while maintaining numerical stability near x = 0.
#'
#' @examples
#' alf(0)
#' alf(c(-4, -1, 0, 1, 4))
#' alf(0.5, eps = 1e-6)
#' @export
alf <- function(x, eps = .001) {
    (x^2 + eps)^.25
}

gr_alf <- function(v, eps = .001) {
    v / (2 * (v^2 + eps)^.75)
}

#' @rdname loss
#'
#' @details L0a, x^2/(x^2 + eps), is an approximation of the L0 penalty.
#'
#' @examples
#' l0a(0)
#' l0a(c(0, 1e-3, 0.1, 1))
#' l0a(c(-2, 0, 2), eps = 1e-4)
#' @export
l0a <- function(x, eps = .01) {
    x^2 / (x^2 + eps)
}

gr_l0a <- function(v, eps = .01) {
    2 * v * eps / (v^2 + eps)^2
}

# Penalized objective function
penalized_obj <- function(
    x,
    obj_fn,
    w,
    pen_fn,
    pen_par_id = NULL,
    pen_diff_id = NULL
) {
    out <- obj_fn(x)
    if (!is.null(pen_par_id)) {
        out <- out + w * sum(pen_fn(x[pen_par_id]))
    }
    if (!is.null(pen_diff_id)) {
        trans_diff <- rep(list(identity), length(pen_diff_id))
        if (any(grepl("^loading", names(pen_diff_id)))) {
            trans_diff[[grep("^loading", names(pen_diff_id))]] <- log
        }
        pen_diff <- Map(
            function(mat, trans) {
                x_mat <- matrix(
                    x[mat],
                    nrow = nrow(mat),
                    ncol = ncol(mat)
                )
                composite_pair_loss(x_mat, fun = pen_fn, trans = trans)
            },
            mat = pen_diff_id,
            trans = trans_diff
        )
        out <- out + w * sum(unlist(pen_diff))
    }
    out
}

#' Penalized Parameter Estimation for Longitudinal CFA Models
#'
#' Performs penalized estimation on a lavaan model object by optimizing a
#' penalized objective function. The function extracts the objective function
#' from a lavaan model, applies a penalty function to the difference in the
#' loading and intercept parameters, and returns an updated model with
#' the optimized parameter estimates.
#'
#' @param x A fitted lavaan model object from which estimation components will
#'   be extracted.
#' @param w Penalty weights applied to the parameters.
#' @param pen_par_id Integer vector of parameter IDs to apply the penalty function
#'   directly to, in the same order as returned by `lavaan::coef()` and by
#'   [lavaan::partable()], with only the free elements.
#' @param pen_diff_id List of matrices containing parameter IDs. For each matrix,
#'   the penalty is applied to the pairwise differences of parameters in the same
#'   column indicated by the IDs.
#' @param pen_fn A character string ("l0a" or "alf") or a function that computes
#'   the penalty. Default is `"l0a"`.
#' @param pen_gr A function that computes the gradient of the penalty function.
#'   If `pen_fn` is "l0a" or "alf", this is automatically set.
#' @param opt_control A list of control parameters passed to `nlminb()` optimizer.
#'   Default includes `eval.max = 2e4`, `iter.max = 1e4`, and `abs.tol = 1e-20`.
#'
#' @return A lavaan model object updated with the penalized parameter estimates.
#'   The returned object includes an attribute `opt_info` containing the
#'   optimization information returned by `nlminb()`.
#'
#' @details
#' The function uses `nlminb()` to minimize a penalized objective function that
#' combines the standard lavaan objective function with a penalty term. Only the
#' parameter estimates and the log-likelihood should be interpreted. The
#' returned object was not "fitted" (`do.fit = FALSE`) to avoid users
#' interpreting the standard errors, which are generally not valid with
#' penalized estimation. The degrees of freedom may also be inaccurate. If the
#' optimization does not converge (convergence code != 0), a warning is issued.
#'
#' @seealso \code{\link[lavaan]{lavaan}}, \code{\link[stats]{nlminb}}
#'
#' @importFrom stats nlminb
#' @examples
#' \dontrun{
#' library(lavaan)
#'
#' # Fit a longitudinal factor model using PoliticalDemocracy data
#' ind_mat <- cbind(c("y1", "y2", "y3", "y4"), c("y5", "y6", "y7", "y8"))
#' fit <- longcfa(ind_mat, lv_names = c("dem60", "dem65"), data = PoliticalDemocracy,
#'                long_equal = c("loadings", "intercepts"), lag_cov = TRUE)
#' # Obtain an unidentified model
#' mod_un <- longcfa_syntax(
#'     ind_mat, lv_names = c("dem60", "dem65"),
#'     lag_cov = TRUE,
#'     free_latvars = TRUE, free_latmeans = TRUE
#' )
#' fit_un <- cfa(mod_un, data = PoliticalDemocracy, do.fit = FALSE, std.lv = TRUE,
#'               start = fit)
#'
#' # Get parameter IDs for loadings
#' load_ids <- get_lav_par_id(fit_un, op = "=~", ind_matrix = ind_mat)
#' int_ids <- get_lav_par_id(fit_un, op = "~1", ind_matrix = ind_mat)
#'
#' # Apply penalized estimation with alignment loss
#' pen_fit <- penalized_est(
#'     x = fit_un,
#'     w = 0.1,
#'     pen_diff_id = list(cbind(t(load_ids), t(int_ids))),
#'     pen_fn = "alf"
#' )
#'
#' # Compare parameter estimates
#' cbind(coef(fit), coef(pen_fit))
#'
#' # Compare log-likelihoods
#' c("scalar invariance" = logLik(fit), "penalized" = logLik(pen_fit))
#' }
#'
#' @importFrom stats update
#' @export
penalized_est <- function(
    x,
    w,
    pen_par_id = NULL,
    pen_diff_id = NULL,
    pen_fn = "l0a",
    pen_gr = NULL,
    opt_control = list(
        eval.max = 2e4,
        iter.max = 1e4,
        abs.tol = 1e-20
    )
) {
    ff <- lavaan::lav_export_estimation(x)
    if (pen_fn %in% c("l0a", "alf")) {
        pen_gr <- switch(
            pen_fn,
            l0a = gr_l0a,
            alf = gr_alf
        )
        pen_fn <- get(pen_fn)
    }
    f1 <- function(v) {
        penalized_obj(
            v,
            obj_fn = function(pars) {
                ff$objective_function(pars, lavaan_model = x)
            },
            w = w,
            pen_fn = pen_fn,
            pen_par_id = pen_par_id,
            pen_diff_id = pen_diff_id
        )
    }
    gr1 <- function(v) {
        penalized_gr(
            v,
            gr_fn = function(pars) ff$gradient_function(pars, lavaan_model = x),
            w = w,
            pen_gr = pen_gr,
            pen_par_id = pen_par_id,
            pen_diff_id = pen_diff_id
        )
    }
    opt <- nlminb(
        ff$starting_values,
        objective = f1,
        gradient = gr1,
        control = opt_control
    )
    if (opt$convergence != 0) {
        warning(
            "Optimization did not converge. Try using better starting values, ",
            "or adjusting optimization control parameters."
        )
    }
    out <- lavaan::lavaan(
        lavaan::partable(x),
        slotOptions = x@Options,
        slotSampleStats = x@SampleStats,
        slotData = x@Data,
        do.fit = FALSE,
        start = opt$par
    )
    attr(out, "opt_info") <- opt
    out
}

penalized_gr <- function(
    x,
    gr_fn,
    w,
    pen_gr,
    pen_par_id = NULL,
    pen_diff_id = NULL,
    ...
) {
    out <- gr_fn(x)
    if (!is.null(pen_par_id)) {
        out <- out + w * hot_gr(x, pen_par_id, pen_gr, ...)
    }
    if (!is.null(pen_diff_id)) {
        trans_diff <- rep(list(identity), length(pen_diff_id))
        gr_trans_diff <- rep(list(function(x) 1), length(pen_diff_id))
        if (any(grepl("^loading", names(pen_diff_id)))) {
            trans_diff[[grep("^loading", names(pen_diff_id))]] <- log
            gr_trans_diff[[grep("^loading", names(pen_diff_id))]] <-
                function(x) 1 / x
        }
        pen_diff_gr <- Map(
            function(mat, trans, gr_trans) {
                hot_gr(
                    x,
                    mat,
                    gr_cpl,
                    gr_fun = pen_gr,
                    trans = trans,
                    gr_trans = gr_trans,
                    ...
                )
            },
            mat = pen_diff_id,
            trans = trans_diff,
            gr_trans = gr_trans_diff
        )
        out <- out + w * Reduce(`+`, pen_diff_gr)
    }
    out
}

# Need to write functions for CV (for choosing w) and penalized estimation
# Not sure if CV is meaningful if the log-likelihood does not change
# Can consider w = 0 (no penalty, close to alignment) to w = inf (scalar invariant)
# Also consider strict invariance?
# Try to make functions general, while the defaults focus on growth models
