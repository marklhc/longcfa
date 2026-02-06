#' Wrapper function for longitudinal CFA (Experimental)
#'
#' This function estimates a longitudinal measurement model by first
#' generating model syntax (using the [longcfa_syntax()] function), and
#' then fit the model using [lavaan::cfa()]. An input matrix is needed
#' where the rows are the indicators and the columns are the time points.
#'
#' Currently only supports model with one latent variable at each time
#' point.
#'
#' @param ind_matrix A \eqn{p \times T} matrix of character strings,
#'   where \eqn{p} is the number of indicators and \eqn{T} is the number
#'   of time points. Rows correspond to repeated measures of the same item,
#'   and columns correspond to time points. Elements should be the variable
#'   names in `data`.
#' @param lv_names A vector of names of \eqn{T} latent variables.
#' @param pattern A list of \eqn{T} elements specifying the pattern of which
#'   indicators load on which latent variable. Each element can be a
#'   \eqn{p \times q} matrix, where \eqn{q} is the number of latent
#'   variables and can be different across time points. Alternatively,
#'   each element can be a list of \eqn{q} integer vectors, specifying
#'   which indicators load on which latent variable. If a list of length
#'   1 is provided, the same pattern is used across all time points.
#'   The default is `NULL`, which assumes only one latent variable per
#'   time point, and all indicators load on it.
#' @param model A character string showing additional syntax to be
#'   added to the model. Defaults to `NULL`.
#' @param lag_cov Logical; whether the same indicator is allowed to
#'   correlate over time.
#' @param long_equal A character vector indicating types of parameters
#'   to be constrained equal across time points. This is similar to
#'   the `group.equal` argument in `lavaan::cfa()`. Currently, only
#'   `"loadings"`, `"intercepts"`, `"thresholds"` and `"residuals"`
#'   are supported.
#' @param long_partial A named list of matrices specifying specific
#'   indicators to have different parameter values for a specific time
#'   point. The list should have names "loadings", "intercepts",
#'   "thresholds", and/or "residuals", and each element is a 2-column
#'   matrix where each row specifies the indicator (column 1) and the
#'   time point (column 2) for the parameter to be free. See example
#'   below.
#' @param nthres A matrix specifying the number of thresholds for each
#'   item, in the same dimension as `ind_matrix`.
#' @param fix_theta Logical; whether to fix the unique variances to 1.
#'   This is default for ordered categorical data.
#' @param free_latvars Logical; whether to free all latent variances,
#'   regardless of `long_equal` specifications.
#' @param free_latmeans Logical; whether to free all latent means,
#'   regardless of `long_equal` specifications.
#' @param data,do.fit,ordered,allow.empty.cell Same as in
#'   `lavaan::lavOptions()`.
#' @param ... Other arguments passed to `lavaan::cfa()`, such as `data`.
#'
#' @return A fit object as returned by `lavaan::cfa()`.
#'
#' @importFrom stats var
#' @importFrom utils modifyList
#' @examples
#' library(lavaan)
#' # Indicator matrix
#' spec <- matrix(c(
#'     "y1", "y2", "y3", "y4",
#'     "y5", "y6", "y7", "y8"
#' ), ncol = 2)
#' # Scalar invariance
#' fit <- longcfa(spec,
#'                lv_names = c("dem60", "dem65"),
#'                data = PoliticalDemocracy,
#'                long_equal = c("loadings", "intercepts"))
#' summary(fit)
#' # Partial invariance
#' fit2 <- longcfa(spec,
#'                 lv_names = c("dem60", "dem65"),
#'                 data = PoliticalDemocracy,
#'                 long_equal = c("loadings", "intercepts"),
#'                 long_partial = list(
#'                     loadings = matrix(c(1, 2), ncol = 2),
#'                     intercepts = matrix(c(1, 3, 2, 2), ncol = 2)
#'                 ))
#' summary(fit2)
#' @export
longcfa <- function(
    ind_matrix,
    lv_names,
    pattern = NULL,
    ucov_mat = NULL,
    data = NULL,
    model = NULL,
    ordered = NULL,
    allow.empty.cell = FALSE,
    lag_cov = FALSE,
    long_equal = NULL,
    long_partial = NULL,
    nthres = NULL,
    fix_theta = NULL,
    free_latvars = FALSE,
    free_latmeans = FALSE,
    do.fit = TRUE,
    ...
) {
    syn_args <- list(
        ind_matrix = ind_matrix,
        lv_names = lv_names,
        pattern = pattern,
        ucov_mat = if (is.null(ucov_mat)) {
            matrix(nrow = 0, ncol = 2)
        } else {
            ucov_mat
        },
        lag_cov = lag_cov,
        long_equal = long_equal,
        long_partial = long_partial,
        free_latvars = free_latvars,
        free_latmeans = free_latmeans
    )

    is_ordered <- !is.null(ordered) && !isFALSE(ordered)
    if (is_ordered) {
        # Build ordered-specific arguments
        ordered_args <- list()
        if (is.null(fix_theta)) {
            ordered_args$fix_theta <- TRUE
        } else {
            ordered_args$fix_theta <- fix_theta
        }

        if (!is.null(data) && is.null(nthres)) {
            nlev <- get_nlev_data(
                data[, unique(sort(ind_matrix)), drop = FALSE],
                ordered = ordered,
                allow.empty.cell = allow.empty.cell
            )
            nthres <- nlev[
                match(ind_matrix, table = names(nlev))
            ] -
                1
            nthres <- structure(pmax(nthres, 0), dim = dim(ind_matrix))
        }

        if (
            "thresholds" %in%
                long_equal &&
                any(apply(nthres, MARGIN = 1, FUN = var) > 0)
        ) {
            stop(
                "Number of thresholds of the same item must be",
                "equal over time."
            )
        }
        ordered_args$nthres <- nthres

        # Merge into syn_args
        syn_args <- modifyList(syn_args, ordered_args)
    }

    syn <- do.call(longcfa_syntax, args = syn_args)

    # Define base arguments with your required defaults
    base_args <- list(...)

    # Define mandatory overrides
    required_args <- list(
        model = c(syn, model),
        data = data,
        do.fit = do.fit,
        ordered = ordered,
        parameterization = if (is_ordered) "theta" else "delta",
        auto.fix.first = FALSE,
        int.lv.free = TRUE
    )

    # Merge: base_args are overridden by required_args
    final_args <- modifyList(base_args, required_args)

    do.call(lavaan::cfa, final_args)
}

# TODO: Accomodate input pattern matrix for multiple latent variables
# Use list instead of matrix for long_partial?
# - Or a data.frame with named columns? item, group (list column), thresholds?

#' @rdname longcfa
#' @param ucov_mat A two-column matrix specifying pairs of indicators
#'   within the same time point to have their residuals correlated. Each row
#'   specifies a pair of indicators by their row indices in `ind_matrix`.
#' @export
longcfa_syntax <- function(
    ind_matrix,
    lv_names,
    pattern = NULL,
    ucov_mat = matrix(nrow = 0, ncol = 2),
    lag_cov = FALSE,
    long_equal = NULL,
    long_partial = NULL,
    nthres = matrix(0, nrow = nrow(ind_matrix), ncol = ncol(ind_matrix)),
    fix_theta = FALSE,
    free_latvars = FALSE,
    free_latmeans = FALSE
) {
    if (
        !is.null(long_partial) &&
            (!is.list(long_partial) ||
                !all(
                    names(long_partial) %in%
                        c("loadings", "intercepts", "thresholds", "residuals")
                ))
    ) {
        stop(
            "`long_partial` must be a named list with names ",
            "'loadings', 'intercepts', 'thresholds', and/or 'residuals'."
        )
    }
    if (
        !is.null(pattern) &&
            (!is.list(pattern) ||
                (length(pattern) != 1 & length(pattern) != ncol(ind_matrix)))
    ) {
        stop(
            "`pattern` must be `NULL`, a list with length 1 to indicate same ",
            "factor pattern across time, or a list with length equal to the ",
            "number of time points for time-specific patterns."
        )
    }
    if (length(pattern) == 1) {
        pattern <- rep(pattern, ncol(ind_matrix))
    }
    nf <- vapply(as.list(lv_names), length, integer(1))
    if (!is.null(long_equal) && any(nf != nf[1])) {
        warning(
            "`long_equal` is likely problematic when the number ",
            "of latent variables is different across time points."
        )
    }
    load_labels <- gen_labels(
        seq_len(nrow(ind_matrix)),
        ncol(ind_matrix),
        cell_len = if (any(nf > 1)) {
            matrix(
                nf,
                nrow = nrow(ind_matrix),
                ncol = ncol(ind_matrix),
                byrow = TRUE
            )
        } else {
            NULL
        },
        prefix = ".l",
        equal = "loadings" %in% long_equal,
        partial = long_partial$loadings
    )
    latvar_syntax <- latent_var_syntax(
        lv_names,
        fix = if (free_latvars || "loadings" %in% long_equal) "first" else "all"
    )
    scalar_inv <- "thresholds" %in% long_equal | "intercepts" %in% long_equal
    ind_cat <- rowMeans(nthres) > 0
    if (!all(ind_cat)) {
        int_labels <- gen_labels(
            seq_len(nrow(ind_matrix)),
            ncol(ind_matrix),
            prefix = ".i",
            equal = "intercepts" %in% long_equal,
            partial = long_partial$intercepts
        )
    } else {
        int_labels <- NULL
    }
    if (any(ind_cat)) {
        thres_labels <- gen_labels(
            seq_len(nrow(ind_matrix)),
            ncol(ind_matrix),
            cell_len = nthres,
            prefix = ".t",
            equal = "thresholds" %in% long_equal,
            partial = long_partial$thresholds
        )
    } else {
        thres_labels <- NULL
    }
    latmean_syntax <- latent_mean_syntax(
        lv_names,
        fix = if (free_latmeans || scalar_inv) "first" else "all"
    )
    uniq_labels <- gen_labels(
        seq_len(nrow(ind_matrix)),
        ncol(ind_matrix),
        prefix = ".u",
        equal = "residuals" %in% long_equal,
        partial = long_partial$residuals
    )
    if (fix_theta) {
        uniq_labels[ind_cat, ] <- "1"
    }
    ucov_labels <- gen_labels(
        paste0(ucov_mat[, 1], ucov_mat[, 2]),
        ncol(ind_matrix),
        prefix = ".uc",
        equal = "residual.covariances" %in% long_equal,
        partial = long_partial$residual.covariances
    )
    syn <- lapply(seq_len(ncol(ind_matrix)), function(t) {
        valid_pos <- which(!is.na(ind_matrix[, t]))
        updated_ucov <- apply(
            ucov_mat,
            1,
            match,
            table = valid_pos,
            simplify = FALSE
        )
        valid_ucov_pos <- which(vapply(
            updated_ucov,
            function(x) all(!is.na(x)),
            logical(1)
        ))
        if (length(valid_ucov_pos) == 0) {
            updated_ucov <- matrix(nrow = 0, ncol = 2)
        } else {
            updated_ucov <- do.call(rbind, updated_ucov[valid_ucov_pos])
        }
        paste0(
            "# Time ",
            t,
            "\n",
            factor_syntax(
                ind_matrix[valid_pos, t, drop = FALSE],
                lv_names = lv_names[[t]],
                pattern = process_pattern(
                    pattern[[t]],
                    ynames = ind_matrix[, t, drop = FALSE]
                )[valid_pos, , drop = FALSE],
                ucov_mat = updated_ucov,
                load_labs = load_labels[valid_pos, t, drop = FALSE],
                int_lab = int_labels[valid_pos, t, drop = FALSE],
                thres_lab = thres_labels[valid_pos, t, drop = FALSE],
                ind_cat = ind_cat[valid_pos],
                uniq_lab = uniq_labels[valid_pos, t, drop = FALSE],
                ucov_lab = ucov_labels[valid_ucov_pos, t, drop = FALSE]
            ),
            "\n"
        )
    })
    if (lag_cov) {
        lagcov_syntax <- lag_cov_syntax(ind_matrix)
    } else {
        lagcov_syntax <- NULL
    }
    paste(
        c(
            syn,
            paste0(latvar_syntax, "\n"),
            paste0(latmean_syntax, "\n"),
            lagcov_syntax
        ),
        collapse = "\n"
    )
}

ld_syntax <- function(ind_names, lv_name, load_lab = NULL) {
    if (is.null(load_lab)) {
        load_ind <- ind_names
    } else {
        load_ind <- paste(load_lab, ind_names, sep = " * ")
    }
    paste0(lv_name, " =~ ", paste0(load_ind, collapse = " + "))
}

one_factor_syntax <- function(
    ind_names,
    lv_name = ".eta",
    load_lab = NULL,
    int_lab = NULL,
    thres_lab = NULL,
    ind_cat = rep(FALSE, length(ind_names)),
    uniq_lab = NULL
) {
    out <- ld_syntax(ind_names, lv_name, load_lab)
    if (!is.null(int_lab)) {
        int_lab <- int_lab[!ind_cat, , drop = FALSE]
        out <- c(out, paste(ind_names[!ind_cat], "~", int_lab, "* 1"))
    }
    if (!is.null(thres_lab)) {
        thres_lab <- thres_lab[ind_cat, , drop = FALSE]
        out <- c(
            out,
            vapply(
                seq_along(ind_names[ind_cat]),
                FUN = function(j) {
                    paste(
                        ind_names[ind_cat][j],
                        "|",
                        paste0(
                            thres_lab[[j]],
                            " * ",
                            "t",
                            seq_along(thres_lab[[j]]),
                            collapse = " + "
                        )
                    )
                },
                FUN.VALUE = character(1)
            )
        )
    }
    if (!is.null(uniq_lab)) {
        out <- c(out, paste(ind_names, "~~", uniq_lab, "*", ind_names))
    }
    paste(out, collapse = "\n")
}

process_pattern <- function(pattern, ynames = NULL) {
    if (is.matrix(pattern)) {
        return(pattern)
    }
    p <- length(ynames)
    if (is.null(pattern)) {
        return(matrix(1, nrow = p))
    } else if (is.list(pattern)) {
        out <- matrix(0, nrow = p, ncol = length(pattern))
        for (l in seq_along(pattern)) {
            if (is.character(l)) {
                yl <- match(pattern[[l]], table = ynames)
                if (any(is.na(yl))) {
                    stop(
                        "Some names in `pattern` are not found in `ind_names`."
                    )
                }
            } else if (is.numeric(l)) {
                yl <- pattern[[l]]
            }
            out[yl, l] <- 1
        }
        return(out)
    }
}

listcol_to_mat <- function(x) {
    matrix(unlist(x), nrow = nrow(x), byrow = TRUE)
}

factor_syntax <- function(
    ind_names,
    lv_names,
    pattern = matrix(1, nrow = length(ind_names)),
    ucov_mat = NULL,
    load_labs = NULL,
    int_lab = NULL,
    thres_lab = NULL,
    ind_cat = rep(FALSE, length(ind_names)),
    uniq_lab = NULL,
    ucov_lab = NULL
) {
    out <- NULL
    load_labs <- listcol_to_mat(load_labs)
    nf <- ncol(pattern)
    for (f in seq_len(nf)) {
        ind_f <- which(pattern[, f] == 1)
        out <- c(
            out,
            ld_syntax(ind_names[ind_f], lv_names[f], load_labs[ind_f, f])
        )
    }
    if (!is.null(int_lab)) {
        int_lab <- int_lab[!ind_cat, , drop = FALSE]
        out <- c(out, paste(ind_names[!ind_cat], "~", int_lab, "* 1"))
    }
    if (!is.null(thres_lab)) {
        thres_lab <- thres_lab[ind_cat, , drop = FALSE]
        out <- c(
            out,
            vapply(
                seq_along(ind_names[ind_cat]),
                FUN = function(j) {
                    paste(
                        ind_names[ind_cat][j],
                        "|",
                        paste0(
                            thres_lab[[j]],
                            " * ",
                            "t",
                            seq_along(thres_lab[[j]]),
                            collapse = " + "
                        )
                    )
                },
                FUN.VALUE = character(1)
            )
        )
    }
    if (!is.null(uniq_lab)) {
        out <- c(out, paste(ind_names, "~~", uniq_lab, "*", ind_names))
    }
    if (isTRUE(nrow(ucov_mat) > 0)) {
        out <- c(
            out,
            paste(
                ind_names[ucov_mat[, 1]],
                "~~",
                ucov_lab,
                "*",
                ind_names[ucov_mat[, 2]]
            )
        )
    }
    paste(out, collapse = "\n")
}

threshold_syntax <- function(ind) {}

lag_cov_syntax <- function(ind_matrix) {
    out <- "# Lag Covariances"
    for (i in seq_len(nrow(ind_matrix))) {
        ind_names_i <- ind_matrix[i, ]
        ind_names_i <- ind_names_i[!is.na(ind_names_i)]
        p_i <- length(ind_names_i)
        for (j in seq_len(p_i - 1)) {
            out <- c(
                out,
                paste(
                    ind_names_i[j],
                    "~~",
                    paste(ind_names_i[(j + 1):p_i], collapse = " + ")
                )
            )
        }
    }
    paste(out, collapse = "\n")
}

gen_labels <- function(
    row_nm,
    nt,
    cell_len = NULL,
    prefix = NULL,
    equal = TRUE,
    partial = NULL,
    use_na_for_partial = FALSE
) {
    out <- matrix(row_nm, nrow = length(row_nm), ncol = nt)
    if (!is.null(cell_len)) {
        out <- lapply(
            seq_along(cell_len),
            FUN = function(l) {
                paste0(out[l], seq_len(cell_len[l]))
            }
        )
        out <- structure(out, dim = c(length(row_nm), nt))
    }
    if (!is.null(prefix)) {
        out[] <- lapply(seq_along(out), function(i) {
            paste0(prefix, out[i][[1]])
        })
        out <- structure(out, dim = c(length(row_nm), nt))
    }
    if (!equal) {
        if (use_na_for_partial) {
            out[] <- "NA"
        } else {
            out[] <- mapply(
                function(x, y) paste(x, y, sep = "_"),
                out,
                rep(seq_len(nt), each = length(row_nm)),
                SIMPLIFY = FALSE
            )
        }
    } else if (!is.null(partial)) {
        partial <- as.matrix(partial)
        partial[, 1] <- match(partial[, 1], table = row_nm)
        for (r in seq_len(nrow(partial))) {
            if (use_na_for_partial) {
                new <- rep("NA", length(out[partial[r, 1], partial[r, 2]])[[1]])
            } else {
                new <- paste(
                    out[partial[r, 1], partial[r, 2]][[1]],
                    partial[r, 2],
                    sep = "_"
                )
            }
            if (is.list(out[partial[r, 1], partial[r, 2]])) {
                new <- list(new)
            }
            out[partial[r, 1], partial[r, 2]] <- new
        }
    }
    structure(out, dim = c(length(row_nm), nt))
}

latent_var_syntax <- function(lv_names, fix = c("first", "all")) {
    fix <- match.arg(fix)
    lv_labs <- rep_len("1", length(lv_names))
    if (fix == "first") {
        lv_labs[-1] <- "NA"
    }
    paste(
        c(
            "# Latent variances",
            # paste(lv_names, "~~", lv_labs, "*", lv_names)
            c(
                mapply(
                    function(x, y) paste(x, "~~", y, "*", x),
                    lv_names,
                    lv_labs
                )
            )
        ),
        collapse = "\n"
    )
}

latent_mean_syntax <- function(lv_names, fix = c("first", "all")) {
    fix <- match.arg(fix)
    lv_labs <- rep_len("0", length(lv_names))
    if (fix == "first") {
        lv_labs[-1] <- "NA"
    }
    paste(
        c(
            "# Latent means",
            # paste(lv_names, "~", lv_labs, "* 1")
            c(
                mapply(
                    function(x, y) paste(x, "~", y, "* 1"),
                    lv_names,
                    lv_labs
                )
            )
        ),
        collapse = "\n"
    )
}

sub_vars <- function(vnames, replacement, x) {
    for (i in seq_along(vnames)) {
        x <- gsub(vnames[i], replacement = replacement[i], x = x)
    }
    x
}
