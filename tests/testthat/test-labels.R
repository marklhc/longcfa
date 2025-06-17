test_that("Equal loadings", {
    lmat <- gen_labels(1:2, 6, prefix = ".l")
    expect_equal(dim(lmat), c(2, 6))
    expect_equal(lmat[, 1], lmat[, 5])
})

test_that("Equal loadings with item subset", {
    lmat <- gen_labels(c(1:3, 5, 7), 3, prefix = ".l")
    expect_equal(dim(lmat), c(5, 3))
})

test_that("Unequal loadings", {
    lmat <- gen_labels(c(1:3, 5, 7), 4, prefix = ".l", equal = FALSE)
    expect_equal(anyDuplicated(c(lmat)), 0)
})

test_that("Allow equality constraints across items", {
    lmat <- gen_labels(c(1, 1, 3, 4), 3, prefix = ".l")
    expect_equal(lmat[1, ], lmat[2, ])
})

test_that("Equal thresholds", {
    thmat <- gen_labels(
        1:3,
        4,
        matrix(c(2, 1, 3), nrow = 3, ncol = 4),
        prefix = ".t"
    )
    expect_equal(dim(thmat), c(3, 4))
    expect_length(thmat[1, 4][[1]], 2)
    expect_equal(thmat[, 2], thmat[, 3])
})
