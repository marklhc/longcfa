get_op_idx <- function(pt, op = c("=~", "~1"), ind_matrix) {
  op <- match.arg(op)
  ind_col <- switch(op, "=~" = "rhs", "~1" = "lhs")
  pt_sub <- pt[pt[["op"]] == op, , drop = FALSE]
  idx <- match(ind_matrix, pt_sub[[ind_col]])
  pt_sub$free[idx]
}

#' Convert Parameter Vector to Matrix
#'
#' Converts a parameter vector to a matrix based on a parameter table and
#' indicator matrix structure.
#'
#' @param x Numeric vector of parameter values.
#' @param op Character string specifying the operator type. Either `"=~"`
#'   (loadings) or `"~1"` (intercepts).
#' @param pt Parameter table, typically from `lavaan::partable()`.
#' @param ind_matrix Matrix defining the structure of indicators. See
#'   [longcfa()] for details on the structure.
#'
#' @return A matrix with dimensions matching `ind_matrix`, filled with parameter
#'   values from `x` according to the free parameter indices.
par_to_mat <- function(x, op, pt, ind_matrix) {
  free_idx <- get_op_idx(pt, op, ind_matrix)
  out <- matrix(NA, nrow = nrow(ind_matrix), ncol = ncol(ind_matrix))
  out[] <- x[free_idx]
  out
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
#' @return A matrix containing the estimated parameters from the lavaan object,
#'   organized according to `ind_matrix`.
#'
#' @export
get_lav_par_mat <- function(
  x,
  op = c("=~", "~1"),
  ind_matrix
) {
  pt <- lavaan::partable(x)
  par_to_mat(pt$est[pt$free != 0], op, pt, ind_matrix)
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
#' @param ... Additional arguments passed to [lavaan::update()].
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
  pt <- lavaan::partable(x)
  pt$ustart[par_id] <- new_start
  pt <- pt[setdiff(names(pt), c("start", "est", "se"))]
  lavaan::update(x, pt, ...)
}
