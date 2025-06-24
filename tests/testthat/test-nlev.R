test_that("Extract correct number of thresholds", {
    expect_equal(
        get_nlev_ordered(
            factor(c(1, 2, 3, 4, NA))
        ),
        4L
    )
    expect_equal(
        get_nlev_ordered(
            rep(2:4, each = 2)
        ),
        3
    )
    f1 <- factor(c(NA, 1, 3, 4, 1), levels = 1:4)
    expect_equal(
        get_nlev_ordered(f1, allow.empty.cell = TRUE),
        4L
    )
    expect_equal(
        get_nlev_ordered(f1, allow.empty.cell = FALSE),
        3L
    )
})
