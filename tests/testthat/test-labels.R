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

test_that("Partial with item subset", {
    lmat <- gen_labels(
        c(1, 3, 7),
        3,
        prefix = ".l",
        partial = matrix(c(7, 1), nrow = 1)
    )
    expect_equal(lmat[3, 1], list(".l7_1"))
})

test_that("array of loading labels", {
    lmats <- gen_labels(
        1:7,
        4,
        prefix = ".l",
        cell_len = matrix(
                        c(3, 2, 2, 2), nrow = 7, ncol = 4,
                    byrow = TRUE),
        partial = matrix(c(3, 2), nrow = 1)
    )
    expect_equal(
        matrix(unlist(lmats[, 2]), nrow = nrow(lmats), byrow = TRUE)[3, 2],
        ".l32_2"
    )
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
