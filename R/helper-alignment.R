#' @importFrom methods slot slotNames
get_op_idx <- function(
    pt,
    op = c("=~", "~1", "|"),
    ind_matrix,
    out_col = "free",
    return_list = "default"
) {
    op <- match.arg(op)
    ind_col <- switch(op, "=~" = "rhs", "~1" = "lhs", "|" = "lhs")
    pt_sub <- pt[pt[["op"]] == op, , drop = FALSE]
    if (op == "|") {
        pt_sub_list <- split(pt_sub, pt_sub$rhs)
    } else {
        pt_sub_list <- list(pt_sub)
    }
    out <- lapply(pt_sub_list, function(df) {
        idx <- match(ind_matrix, df[[ind_col]])
        df[[out_col]][idx]
    })
    if ((return_list == "default" && length(out) == 1) || isTRUE(return_list)) {
        return(unlist(out))
    }
    out
    # idx <- match(ind_matrix, pt_sub[[ind_col]])
    # pt_sub$free[idx]
}

#' Extract Parameter Values to Matrix
#'
#' Internal helper to extract values from a lavaan object's parameter table
#' and organize them into a matrix based on the indicator matrix.
#'
#' @param x A fitted lavaan object.
#' @param op Character string specifying the operator type. One of `"=~"`,
#'   `"~1"`, or `"|"`.
#' @param ind_matrix Matrix defining the structure of indicators. See
#'   [longcfa()] for details.
#' @param out_col Character string specifying the column name in the parameter
#'   table to extract (e.g., `"est"`, `"free"`, `"se"`).
#'
#' @return A matrix with dimensions matching `ind_matrix` (or stacked matrices
#'   for thresholds) containing the  extracted values.
#' @export
par_to_mat <- function(x, op, ind_matrix, out_col) {
    pt <- lavaan::partable(x)
    idx_list <- get_op_idx(
        pt,
        op,
        ind_matrix,
        out_col = out_col,
        return_list = FALSE
    )
    out <- lapply(
        idx_list,
        matrix,
        nrow = nrow(ind_matrix),
        ncol = ncol(ind_matrix)
    )
    do.call(rbind, out)
}

#' @rdname par_to_mat
#'
#' @param pt Parameter table from `lavaan::partable()`. When a lavaan object is
#'   passed, this will be called internally.
par_to_mat_from_pt <- function(pt, op, ind_matrix, out_col) {
    idx_list <- get_op_idx(
        pt,
        op,
        ind_matrix,
        out_col = out_col,
        return_list = FALSE
    )
    out <- lapply(
        idx_list,
        matrix,
        nrow = nrow(ind_matrix),
        ncol = ncol(ind_matrix)
    )
    do.call(rbind, out)
}

#' Extract Parameter Matrix from lavaan Object
#'
#' Extracts parameter estimates from a fitted lavaan object and organizes them
#' into a matrix based on the indicator matrix structure.
#'
#' @param x A fitted lavaan object.
#' @param op Character string specifying the operator type. Either `"=~"`
#'   (loadings) or `"~1"` (intercepts). Defaults to `"=~"`.
#' @param ind_matrix Matrix defining the structure of indicators. See
#'   [longcfa()] for details on the structure.
#'
#' @return A matrix containing the estimated parameters (or IDs) from the
#'   lavaan object, organized according to `ind_matrix`.
#'
#' @export
get_lav_par_mat <- function(x, op = c("=~", "~1", "|"), ind_matrix) {
    par_to_mat(x, op = match.arg(op), ind_matrix, out_col = "est")
}

#' @rdname get_lav_par_mat
#'
#' @details `get_lav_par_id()` extracts the parameter IDs from a fitted lavaan object,
#' based on the specified operator and indicator matrix.
#'
#' @export
get_lav_par_id <- function(x, op = c("=~", "~1", "|"), ind_matrix) {
    par_to_mat(x, op = match.arg(op), ind_matrix, out_col = "free")
}

#' Update User Starting Values for Specific Parameters by ID
#'
#' This function updates the user starting values, or the constrained values,
#' for specific parameters in a lavaan object and refits the model.
#'
#' @param x A fitted lavaan object.
#' @param par_id Integer vector of parameter IDs ("id" column in the parameter
#'   table) to update.
#' @param new_start Numeric vector of new starting values corresponding to
#'   `par_id`.
#' @param ... Additional arguments passed to the `update()` method for lavaan
#'   objects.
#'
#' @return A refitted lavaan object with updated starting values.
#'
#' @examples
#' library(lavaan)
#' # Fit a simple CFA model
#' HS.model <- 'visual =~ x1 + x2 + x3'
#' fit <- cfa(HS.model, data = HolzingerSwineford1939)
#'
#' # View parameter table to identify parameter IDs
#' lavaan::partable(fit)
#'
#' # Update value for the first loading from 1 to 1.5
#' fit_updated <- update_ustart(fit, par_id = 1, new_start = 1.5)
#' lavaan::coef(fit_updated)
#'
#' @export
update_ustart <- function(x, par_id, new_start, ...) {
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
    pt <- lavaan::partable(x)
    pt$ustart[par_id] <- new_start
    pt <- pt[setdiff(names(pt), c("start", "est", "se"))]
    lavaan::lavaan(
        pt,
        slotOptions = x_opt,
        slotSampleStats = x_ss,
        slotData = x_dat,
        ...
    )
}
