# Write a function that computes the penalized log-likelihood of a growth model
# Write a function that computes the gradient (gradient of LL - gradient of penalty)
# Find out how to obtain LL from lavaan
# Obtain analytic gradient from lavaan
# Write function for optimization using optim . . .

composite_pair_loss <- function(x, fun = alf, trans = identity, ...) {
  x <- as.matrix(trans(x))
  combn_idx <- combn(nrow(x), 2)
  out <- fun(x[combn_idx[1, ], ] - x[combn_idx[2, ], ], ...)
  sum(out)
}

alf <- function(x, eps = .001) {
  (x^2 + eps)^.25
}

par_to_mat <- function(x, op = c("=~", "~1"), pt, ind_matrix) {
  op <- match.arg(op)
  ind_col <- switch(op, "=~" = "rhs", "~1" = "lhs")
  pt_sub <- pt[pt[["op"]] == op, , drop = FALSE]
  idx <- match(ind_matrix, pt_sub[[ind_col]])
  out <- matrix(NA, nrow = nrow(ind_matrix), ncol = ncol(ind_matrix))
  out[] <- x[pt_sub$free[idx]]
  out
}

# Need to write functions for CV (for choosing w) and penalized estimation
# Not sure if CV is meaningful if the log-likelihood does not change
# Can consider w = 0 (no penalty, close to alignment) to w = inf (scalar invariant)
# Also consider strict invariance?
# Try to make functions general, while the defaults focus on growth models