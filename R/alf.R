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

gr_cpl_alf <- function(x, eps = .001) {
  x_mat <- as.matrix(x)
  combn_idx <- combn(nrow(x_mat), 2)
  diffs <- x_mat[combn_idx[1, ], , drop = FALSE] -
    x_mat[combn_idx[2, ], drop = FALSE]
  signs <- sign(diffs)
  denom <- 2 * (diffs^2 + eps)^.75
  grad_contribs <- diffs / denom
  grad <- matrix(0, nrow = nrow(x_mat), ncol = ncol(x_mat))
  for (i in seq_len(nrow(x_mat))) {
    idx1 <- which(combn_idx[1, ] == i)
    idx2 <- which(combn_idx[2, ] == i)
    grad[i, ] <- colSums(grad_contribs[idx1, , drop = FALSE]) -
      colSums(grad_contribs[idx2, , drop = FALSE])
  }
  as.vector(grad)
}

alf <- function(x, eps = .001) {
  (x^2 + eps)^.25
}

# Penalized objective function
penalized_obj <- function(
  x,
  obj_fn,
  pt,
  ind_matrix,
  w,
  pen_fn = composite_pair_loss,
  pen_op = c("=~", "~1")
) {
  par_mats <- lapply(
    pen_op,
    function(op) par_to_mat(x, op, pt, ind_matrix)
  )
  obj_fn(x) +
    w *
      Reduce(
        function(acc, mat) acc + pen_fn(t(mat)),
        par_mats,
        init = 0
      )
}

penalized_est <- function(
  x,
  w,
  ind_matrix,
  pen_fn = composite_pair_loss,
  pen_op = c("=~", "~1"),
  opt_control = list(
    eval.max = 2e4,
    iter.max = 1e4,
    abs.tol = 1e-20
  ),
  data2 = NULL
) {
  ff <- lavaan::lav_export_estimation(x)
  opt <- nlminb(
    ff$starting_values,
    objective = penalized_obj,
    obj_fn = function(pars) ff$objective_function(pars, lavaan_model = x),
    pt = lavaan::partable(x),
    ind_matrix = ind_matrix,
    w = w,
    pen_fn = pen_fn,
    pen_op = pen_op,
    control = opt_control
  )
  if (opt$convergence != 0) {
    warning("Optimization did not converge.")
  }
  pfit1 <- update(x, start = opt$par, data = x@Data)
  if (!is.null(data2)) {
    pfit2 <- update(x, start = opt$par, data = data2)
  } else {
    pfit2 <- NULL
  }
  list(
    opt = opt,
    pfit1 = pfit1,
    pfit2 = pfit2
  )
}

# Need to write functions for CV (for choosing w) and penalized estimation
# Not sure if CV is meaningful if the log-likelihood does not change
# Can consider w = 0 (no penalty, close to alignment) to w = inf (scalar invariant)
# Also consider strict invariance?
# Try to make functions general, while the defaults focus on growth models
