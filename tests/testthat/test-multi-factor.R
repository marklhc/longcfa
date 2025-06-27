test_that("multiple factors", {
    pat <- list(1:3, 4:6, 6:7)
    lvs <- c("a2", "b2", "c2")
    partials <- list(
        matrix(c(1, 2), nrow = 1),
        matrix(nrow = 0, ncol = 2),
        matrix(c(6, 7, 2, 3), nrow = 2)
    )
    larr <- gen_labels(
        1:7,
        4,
        prefix = ".l",
        cell_len = matrix(3, nrow = 7, ncol = 4,
                    byrow = TRUE),
        partial = matrix(c(7, 5, 2, 3), ncol = 2)
    )
    expect_equal(larr[7, 2][[1]][2], ".l72_2")
    mf_syntax <- factor_syntax(
        paste0("y", 1:7),
        lvs,
        process_pattern(pat, ynames = 1:7),
        larr[, 2, drop = FALSE],
        int_lab = matrix("NA", nrow = 7, ncol = 1)
    )
    expect_match(
        mf_syntax,
        "b2 =~ .l42 * y4 + .l52 * y5 + .l62 * y6",
        fixed = TRUE
    )
})

test_that("one factor", {
    lmat <- gen_labels(
        1:4,
        3,
        prefix = ".l",
        partial = matrix(c(3, 2), nrow = 1)
    )
    imat <- gen_labels(
        1:4,
        3,
        prefix = ".i",
        partial = matrix(c(3, 2), nrow = 1)
    )
    of_syntax <- factor_syntax(
        paste0("y", 1:4),
        ".eta2",
        load_labs = lmat[, 2, drop = FALSE],
        int_lab = imat[, 2, drop = FALSE]
    )
    expect_match(
        of_syntax,
        "eta2 =~ .l1 * y1 + .l2 * y2 + .l3_2 * y3",
        fixed = TRUE
    )
})
