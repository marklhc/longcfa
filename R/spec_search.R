next_to_relax <- function(x, fn, fn_min, ...) {
    fn_x <- fn(x, ...)
    # A search stage can reach a state with no candidates left, in which case
    # the candidate function may return a non-data.frame. Guard so `$mi` is
    # never called on such a value (which raises "$ operator is invalid for
    # atomic vectors") and stop the stage with an empty frame.
    if (is.null(fn_x) || !is.data.frame(fn_x) || nrow(fn_x) == 0L) {
        return(if (is.data.frame(fn_x)) fn_x else data.frame())
    }
    if (all(fn_x$mi < fn_min)) {
        return(fn_x[NULL, ])
    }
    fn_x[which.max(fn_x$mi), ]
}

filter_pt <- function(pt, ind, op = c("=~", "~1", "~~", "|")) {
    op <- match.arg(op)
    if (op == "=~") {
        return(pt[pt$rhs %in% ind & pt$op == "=~", ])
    } else if (op == "~1") {
        return(pt[pt$lhs %in% ind & pt$op == "~1", ])
    } else if (op == "~~") {
        return(pt[pt$lhs %in% ind & pt$rhs %in% ind & pt$op == "~~", ])
    } else if (op == "|") {
        return(pt[pt$lhs %in% ind & pt$op == "|", ])
    }
}

filter_cons <- function(pt, cons) {
    pt1 <- pt[pt$label != "" & pt$free > 0, ]
    which(cons$lhs %in% pt1$plabel | cons$rhs %in% pt1$plabel)
}

#' Compute Score Tests for Equality Constraints
#'
#' Computes score tests (Lagrange Multiplier tests) for releasing equality
#' constraints on specific parameters.
#'
#' @param x A fitted lavaan object.
#' @param ind Character vector of indicator names to consider.
#' @param op Character string specifying the operator type (`"=~"`, `"~1"`, `"~~"`, or `"|"`).
#'
#' @return A data frame containing the score test results, including the
#'   modification index (`mi`) and the p-value (`p`) for each constraint.
#' @export
get_lav_test_score <- function(x, ind, op = c("=~", "~1", "~~", "|")) {
    pt <- partable(x)
    pt_op <- filter_pt(pt, ind, op)
    to_test <- filter_cons(pt_op, pt[pt$op == "==", ])
    if (length(to_test) == 0) {
        return(data.frame(
            id = integer(0),
            lhs = character(0),
            op = character(0),
            rhs = character(0),
            group = integer(0),
            plabel = character(0),
            mi = numeric(0),
            p = numeric(0)
        ))
    }
    scores <- lavaan::lavTestScore(x, release = to_test)$uni
    pt_test <- pt_op[
        match(scores$rhs, pt_op$plabel),
        c("id", "lhs", "op", "rhs", "group", "plabel")
    ]
    cbind(pt_test, mi = scores$X2, p = scores$p.value)
}

#' Compute Modification Indices for Specific Parameters
#'
#' Computes modification indices for specific parameters restricted by
#' indicator names and operator type.
#'
#' @param x A fitted lavaan object.
#' @param ind Character vector of indicator names to consider.
#' @param op Character string specifying the operator type (`"=~"`, `"~1"`, `"~~"`, or `"|"`).
#'
#' @return A data frame containing the modification index (`mi`) and the
#'   p-value (`p`) for each candidate parameter.
#' @export
get_lav_mod <- function(x, ind, op = c("=~", "~1", "~~", "|")) {
    mis <- lavaan::modindices(x, free.remove = FALSE)
    out <- filter_pt(mis, ind, op)
    # lavaan >= 0.7 no longer returns a `p` column from modindices();
    # each mi is a 1-df chi-squared statistic, so compute p from it
    if (!"p" %in% names(out)) {
        out$p <- stats::pchisq(out$mi, df = 1, lower.tail = FALSE)
    }
    merge(
        out[c("lhs", "op", "rhs", "mi", "p")],
        partable(x)[c("id", "lhs", "op", "rhs", "group", "plabel")]
    )
}

#' Compute 1-df Likelihood Ratio Tests for Equality Constraints
#'
#' Computes 1-df likelihood ratio tests (LRTs) for releasing equality
#' constraints on specific parameters, where each tied constraint is
#' released individually and the model is refit.
#'
#' @param x A fitted lavaan object.
#' @param ind Character vector of indicator names to consider.
#' @param op Character string specifying the operator type (`"=~"`, `"~1"`, `"~~"`, or `"|"`).
#'
#' @return A data frame with one row for each tied equality constraint,
#'   containing `id`, `lhs`, `op`, `rhs`, `group`, `plabel`, the 1-df LRT
#'   statistic for releasing the constraint (`mi`), and its p-value (`p`).
#'   For `op = "~1"` and `op = "|"`, `rhs` is empty and the variable is in
#'   `lhs`. A data frame with 0 rows (and the same column names) is
#'   returned when no candidate is tied.
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
#' # 1-df LRTs for releasing each tied intercept constraint
#' get_lav_lrt(fit, ind = paste0("y", 1:8), op = "~1")
#' @export
get_lav_lrt <- function(x, ind, op = c("=~", "~1", "~~", "|")) {
    pt <- partable(x)
    pt_eq <- pt[pt$op == "==", ]
    pt_op <- filter_pt(pt, ind, op)
    to_test <- filter_cons(pt_op, pt_eq)
    if (length(to_test) == 0) {
        return(data.frame(
            id = integer(0),
            lhs = character(0),
            op = character(0),
            rhs = character(0),
            group = integer(0),
            plabel = character(0),
            mi = numeric(0),
            p = numeric(0)
        ))
    }
    required_slots <- c("Options", "SampleStats", "Data")
    missing_slots <- setdiff(required_slots, slotNames(x))
    if (length(missing_slots) > 0L) {
        stop(
            "lavaan object is missing expected slots: ",
            paste(missing_slots, collapse = ", "),
            call. = FALSE
        )
    }
    x_opt <- slot(x, "Options")
    x_ss <- slot(x, "SampleStats")
    x_dat <- slot(x, "Data")
    # each candidate is one `==` row; releasing either endpoint plabel
    # removes that constraint (1 df)
    eq_test <- pt_eq[to_test, ]
    # refit per candidate (O(#candidates) fits; models are small)
    lrt_out <- vapply(
        seq_len(nrow(eq_test)),
        function(j) {
            pt_new <- lav_constraints_rm(pt, eq_test$rhs[j])
            x_new <- lavaan::lavaan(
                pt_new,
                slotOptions = x_opt,
                slotData = x_dat,
                slotSampleStats = x_ss
            )
            lrt_i <- lavaan::lavTestLRT(x, x_new)
            c(
                mi = unname(lrt_i[2, "Chisq diff"]),
                p = unname(lrt_i[2, "Pr(>Chisq)"])
            )
        },
        FUN.VALUE = c(mi = numeric(1), p = numeric(1))
    )
    cbind(
        pt_op[
            match(eq_test$rhs, pt_op$plabel),
            c("id", "lhs", "op", "rhs", "group", "plabel")
        ],
        mi = lrt_out["mi", ],
        p = lrt_out["p", ]
    )
}

#' @importFrom lavaan partable
#' @importFrom methods slot slotNames
plinv_search_step <- function(
    x,
    op = c("=~", "~1", "~~", "|"),
    mi_fun,
    mi_min,
    ...,
    cutoffs = NULL,
    check_min2 = FALSE,
    ind_matrix = NULL
) {
    new_x <- x
    required_slots <- c("Options", "SampleStats", "Data")
    missing_slots <- setdiff(required_slots, slotNames(x))
    if (length(missing_slots) > 0L) {
        stop(
            "lavaan object is missing expected slots: ",
            paste(missing_slots, collapse = ", "),
            call. = FALSE
        )
    }
    x_opt <- slot(x, "Options")
    x_ss <- slot(x, "SampleStats")
    x_dat <- slot(x, "Data")
    free_trace <- list()
    k <- 1L
    while (TRUE) {
        # min2 guard: stop the stage before a release if 2 or fewer
        # items are still tied for this op (needs ind_matrix)
        if (
            check_min2 &&
            !is.null(ind_matrix) &&
            count_tied_inds(partable(new_x), op, ind_matrix) <= 2L
        ) {
            break
        }
        # one cutoff per iteration; stop once `cutoffs` is exhausted
        if (!is.null(cutoffs) && k > length(cutoffs)) {
            break
        }
        k_cut <- if (is.null(cutoffs)) mi_min else cutoffs[k]
        to_free <- next_to_relax(
            new_x,
            fn = mi_fun,
            fn_min = k_cut,
            op = op,
            ...
        )
        if (nrow(to_free) == 0) {
            break
        }
        free_trace[[length(free_trace) + 1L]] <- to_free
        plab <- to_free$plabel
        pt <- partable(new_x)
        pt_new <- lav_constraints_rm(pt, plab)
        new_x <- lavaan::lavaan(
            pt_new,
            slotOptions = x_opt,
            slotData = x_dat,
            slotSampleStats = x_ss
        )
        k <- k + 1L
    }
    trace_out <- if (length(free_trace) > 0) {
        do.call(rbind, free_trace)
    } else {
        NULL
    }
    list(fit = new_x, trace = trace_out)
}

# Count the items (rows of ind_matrix) still tied by equality
# constraints of the given op type in a fitted model: an item counts as
# tied when any of its per-time-point variable names has a partable row
# of this op with free > 0 whose plabel appears in a `==` row
count_tied_inds <- function(pt, op = c("=~", "~1", "~~", "|"), ind_matrix) {
    op <- match.arg(op)
    ind_col <- switch(
        op,
        "=~" = "rhs",
        "~1" = "lhs",
        "~~" = "lhs",
        "|" = "lhs"
    )
    eq_plabels <- unlist(pt[pt$op == "==", c("lhs", "rhs")],
                        use.names = FALSE)
    row_tied <- pt$op == op & pt$free > 0 & pt$plabel %in% eq_plabels
    tied_vars <- pt[[ind_col]][row_tied]
    sum(vapply(
        seq_len(nrow(ind_matrix)),
        function(i) any(ind_matrix[i, ] %in% tied_vars),
        logical(1)
    ))
}

#' Specification Search for Partial Invariance
#'
#' Performs a specification search for partial longitudinal invariance by
#' iteratively freeing parameters with high modification indices or score
#' test statistics, or other user-defined criteria.
#'
#' @param ind_matrix Matrix defining the structure of indicators. See
#'   [longcfa()] for details on the structure.
#' @param lv_names A character vector of names for the latent variables.
#' @param data A data frame containing the observed variables.
#' @param type A character vector specifying the types of parameters to search
#'   for partial invariance. Supported types are `"loadings"`, `"intercepts"`,
#'   `"thresholds"`, `"residuals"`, and `"residual.covariances"`. The search
#'   is performed sequentially in the order specified.
#' @param mi_fun A function to compute modification indices or score tests.
#'   Common choices are [get_lav_test_score()] (for score tests on equality
#'   constraints) or [get_lav_mod()] (for modification indices), but other
#'   functions can be used as long as they return a similar data frame with
#'   columns of `lhs`, `op`, `rhs`, `id`, and `plabel` as defined in
#'   [lavaan::parTable()], and a column of `mi`.
#' @param mi_min A numeric value specifying the minimum threshold for the
#'   modification index or score test statistic to free a parameter.
#'   Ignored when `control_fdr = TRUE`.
#' @param control_fdr Logical; whether to control the false discovery rate
#'   for multiple testing. If `TRUE`, instead of a fixed `mi_min`, the
#'   k-th freed parameter at each stage must have a test statistic above a
#'   Benjamini and Gavrilov (2009) adjusted threshold, computed as
#'   `qchisq(pinsearch::fdr_alpha(k, m, q = sig_level), 1,
#'   lower.tail = FALSE)`, where `m` is the number of tied candidates at
#'   the start of the stage.
#' @param sig_level Significance level (target false discovery rate) used
#'   when `control_fdr = TRUE`. Default is .05.
#' @param min2 Logical; whether to stop a stage when 2 or fewer items are
#'   still tied for the parameter type, analogous to `min2` in
#'   [pinsearch::pinSearch()]. Applies only to `type` values `"loadings"`,
#'   `"intercepts"`, and `"thresholds"`.
#' @param ... Additional arguments passed to [longcfa()].
#'
#' @return A list containing:
#'   \item{fit}{The final fitted lavaan object with partial invariance constraints.}
#'   \item{traces}{A data frame tracking the parameters freed during the search process, with one row per freed parameter and the columns returned by `mi_fun` (including the test statistic `mi` and the p-value `p` for the built-in candidates).}
#'
#' @references Benjamini, Y. & Gavrilov, N. M. (2009). Sequential selection
#'   procedures for testing dependent hypotheses.
#'
#' @seealso [pinsearch::pinSearch()] for cross-sectional (multi-group)
#'   specification search.
#'
#' @export
plinv_search <- function(
    ind_matrix,
    lv_names,
    data,
    type,
    mi_fun,
    mi_min,
    control_fdr = FALSE,
    sig_level = .05,
    min2 = FALSE,
    ...
) {
    traces <- list()
    if (
        !all(
            type %in%
                c(
                    "loadings",
                    "intercepts",
                    "thresholds",
                    "residuals",
                    "residual.covariances"
                )
        )
    ) {
        stop(
            "type must be one of 'loadings', 'intercepts', 'thresholds'",
            "'residuals', or 'residual.covariances'"
        )
    }
    eq_lst <- NULL
    part_lst <- NULL
    inds <- c(ind_matrix)
    for (type_i in type) {
        op <- pinsearch::type2op(type_i)
        eq_lst <- c(eq_lst, type_i)
        if (type_i == "intercepts") {
            part_lst$intercepts <- part_lst$loadings
        }
        x_base <- longcfa(
            ind_matrix,
            lv_names = lv_names,
            data = data,
            long_equal = eq_lst,
            long_partial = part_lst,
            ...
        )
        # number of tied candidates at the start of the stage
        n_cand <- nrow(mi_fun(x_base, op = op, ind = inds))
        # per-iteration mi cutoffs; `mi_min` is ignored entirely when
        # `control_fdr` is TRUE
        cutoffs <- if (control_fdr) {
            if (n_cand > 0) {
                stats::qchisq(
                    vapply(
                        seq_len(n_cand),
                        function(k) pinsearch::fdr_alpha(
                            k, n_cand, q = sig_level
                        ),
                        numeric(1)
                    ),
                    df = 1,
                    lower.tail = FALSE
                )
            } else {
                numeric(0)
            }
        } else {
            NULL
        }
        # min2 applies to the metric and scalar invariance stages only
        check_min2 <-
            min2 && type_i %in% c("loadings", "intercepts", "thresholds")
        if (n_cand > 0) {
            res_i <- plinv_search_step(
                x_base,
                op = op,
                mi_fun = mi_fun,
                mi_min = mi_min,
                ind = inds,
                cutoffs = cutoffs,
                check_min2 = check_min2,
                ind_matrix = ind_matrix
            )
        } else {
            # nothing tied at stage start: skip the search and record an
            # empty trace, as before
            res_i <- list(fit = x_base, trace = NULL)
        }
        traces[[length(traces) + 1L]] <- res_i$trace
        part_lst[[type_i]] <- rbind(
            part_lst[[type_i]],
            partial_string_to_list(
                pt_to_partial_string(res_i$trace),
                ind_matrix
            )[[type_i]]
        )
    }
    traces_out <- if (length(traces) > 0) do.call(rbind, traces) else NULL
    list(fit = res_i$fit, traces = traces_out)
}

lav_constraints_rm <- function(pt, plab) {
    eq_rows <- which(pt$op == "==")
    # Identify rows with the specified label on lhs or rhs (pre-filtered)
    idl <- pt$id[eq_rows[pt$lhs[eq_rows] == plab]]
    idr <- pt$id[eq_rows[pt$rhs[eq_rows] == plab]]
    if (length(idl) == 0 && length(idr) == 0) {
        return(pt)
    }
    # Find all labels associated with the specified label
    eq_plabs <- union(
        pt$rhs[pt$id %in% idl],
        pt$lhs[pt$id %in% idr]
    )
    next_plab <- eq_plabs[eq_plabs != plab][1]
    pt_new <- pt
    # Replace all occurrences of plab with next_plab
    pt_new$lhs[pt_new$lhs == plab] <- next_plab
    pt_new$rhs[pt_new$rhs == plab] <- next_plab
    # Reverse lhs and rhs
    pt_rev <- data.frame(lhs = pt_new$rhs, rhs = pt_new$lhs)
    # Find all duplicated constraints
    id_to_rm <- which(
        pt_new$op == "==" & pt_new$lhs == pt_rev$lhs & pt_new$rhs == pt_rev$rhs
    )
    pt_new[-id_to_rm, ]
}
