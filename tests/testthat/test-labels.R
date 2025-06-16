test_that("generate labels properly", {
    lmat <- gen_labels(c(2, 6), prefix = ".l")
    expect_equal(dim(lmat), c(2, 6))
    expect_equal(lmat[, 1], lmat[, 5])
})
