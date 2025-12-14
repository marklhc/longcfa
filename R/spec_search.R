next_to_relax <- function(x, fn, fn_min, ...) {
    fn_x <- fn(x, ...)
    if (all(fn_x$mi < fn_min)) {
        return(fn_x[NULL, ])
    }
    fn_x[which.max(fn_x$mi), ]
}

filter_pt <- function(pt, ind, op = c("=~", "~1", "~~")) {
    op <- match.arg(op)
    if (op == "=~") {
        return(pt[pt$rhs %in% ind & pt$op == "=~", ])
    } else if (op == "~1") {
        return(pt[pt$lhs %in% ind & pt$op == "~1", ])
    } else if (op == "~~") {
        return(pt[pt$lhs %in% ind & pt$rhs %in% ind & pt$op == "~~", ])
    }
}

filter_cons <- function(pt, cons) {
    pt1 <- pt[pt$label != "" & pt$free >= 0, ]
    which(cons$lhs %in% pt1$plabel | cons$rhs %in% pt1$plabel)
}

#' Compute Score Tests for Equality Constraints
#'
#' Computes score tests (Lagrange Multiplier tests) for releasing equality
#' constraints on specific parameters.
#'
#' @param x A fitted lavaan object.
#' @param ind Character vector of indicator names to consider.
#' @param op Character string specifying the operator type (`"=~"`, `"~1"`, or `"~~"`).
#'
#' @return A data frame containing the score test results, including the
#'   modification index (`mi`) for each constraint.
#' @export
get_lav_test_score <- function(x, ind, op = c("=~", "~1", "~~")) {
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
            mi = numeric(0)
        ))
    }
    scores <- lavaan::lavTestScore(x, release = to_test)$uni
    pt_test <- pt_op[
        match(scores$rhs, pt_op$plabel),
        c("id", "lhs", "op", "rhs", "group", "plabel")
    ]
    cbind(pt_test, mi = scores$X2)
}

#' Compute Modification Indices for Specific Parameters
#'
#' Computes modification indices for specific parameters restricted by
#' indicator names and operator type.
#'
#' @param x A fitted lavaan object.
#' @param ind Character vector of indicator names to consider.
#' @param op Character string specifying the operator type (`"=~"`, `"~1"`, or `"~~"`).
#'
#' @return A data frame containing the modification indices.
#' @export
get_lav_mod <- function(x, ind, op = c("=~", "~1", "~~")) {
    mis <- lavaan::modindices(x, free.remove = FALSE)
    out <- filter_pt(mis, ind, op)
    merge(
        out[c("lhs", "op", "rhs", "mi")],
        partable(x)[c("id", "lhs", "op", "rhs", "group", "plabel")]
    )
}

#' @importFrom lavaan partable
plinv_search_step <- function(
    x,
    op = c("=~", "~1", "~~"),
    mi_fun,
    mi_min,
    ...
) {
    new_x <- x
    x_opt <- x@Options
    x_ss <- x@SampleStats
    x_dat <- x@Data
    free_trace <- NULL
    while (TRUE) {
        to_free <- next_to_relax(
            new_x,
            fn = mi_fun,
            fn_min = mi_min,
            op = op,
            ...
        )
        if (nrow(to_free) == 0) {
            break
        }
        free_trace <- rbind(free_trace, to_free)
        plab <- to_free$plabel
        pt <- partable(new_x)
        pt_new <- lav_constraints_rm(pt, plab)
        new_x <- lavaan::lavaan(
            pt_new,
            slotOptions = x_opt,
            slotData = x_dat,
            slotSampleStats = x_ss
        )
    }
    list(fit = new_x, trace = free_trace)
}

type2op <- function(x) {
    switch(
        x,
        loadings = "=~",
        intercepts = "~1",
        thresholds = "|",
        residuals = "~~",
        residual.covariances = "~~"
    )
}

#' Specification Search for Partial Invariance
#'
#' Performs a specification search for partial longitudinal invariance by
#' iteratively freeing parameters with high modification indices or score
#' test statistics, or other user-defined criteria.
#'
#' @param ind_matrix A character matrix specifying the names of the indicator
#'   variables across time points. Each column corresponds to a time point.
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
#' @param ... Additional arguments passed to [longcfa()].
#'
#' @return A list containing:
#'   \item{fit}{The final fitted lavaan object with partial invariance constraints.}
#'   \item{traces}{A data frame tracking the parameters freed during the search process.}
#'
#' @export
plinv_search <- function(
    ind_matrix,
    lv_names,
    data,
    type,
    mi_fun,
    mi_min,
    ...
) {
    traces <- NULL
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
    for (type_i in type) {
        op <- type2op(type_i)
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
        res_i <- plinv_search_step(
            x_base,
            op = op,
            mi_fun = mi_fun,
            mi_min = mi_min,
            ind = c(ind_matrix)
        )
        traces <- rbind(traces, res_i$trace)
        part_lst[[type_i]] <- rbind(
            part_lst[[type_i]],
            partial_string_to_list(
                pt_to_partial_string(res_i$trace),
                ind_matrix
            )[[type_i]]
        )
    }
    list(fit = res_i[[1]], traces = traces)
}

lav_constraints_rm <- function(pt, plab) {
    # Identify rows with the specified label on lhs or rhs
    idl <- pt$id[which(pt$op == "==" & pt$lhs == plab)]
    idr <- pt$id[which(pt$op == "==" & pt$rhs == plab)]
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
