test_that("generate thresholds labels properly", {
    thmat <- gen_labels2(
        c(3, 4),
        matrix(c(2, 1, 3), nrow = 3, ncol = 4),
        prefix = ".t"
    )
    expect_equal(dim(thmat), c(3, 4))
    expect_length(thmat[1, 4][[1]], 2)
    expect_equal(thmat[, 2], thmat[, 3])
})
