test_that("alf is computed correctly", {
    x <- matrix(c(-2, -1, 0.5, 1, 2), ncol = 1)
    result <- alf(x, eps = 1e-6)
    expected <- sqrt(abs(x))
    expect_equal(result, expected, tolerance = 1e-6)
})

test_that("composite_pair_loss computes correct sum", {
    x1 <- rbind(
        c(1, 1.2, 1.2),
        c(1, 0.6, 0.6),
        c(1, 1.2, 0.9)
    )
    result <- composite_pair_loss(x1, fun = alf, eps = 1e-16)
    expected <- sum(
        sqrt(abs(x1[1, ] - x1[2, ])),
        sqrt(abs(x1[1, ] - x1[3, ])),
        sqrt(abs(x1[2, ] - x1[3, ]))
    )
    expect_equal(result, expected * 2 / 3, tolerance = 1e-3)
    res1 <- composite_pair_loss(x1[, 1], fun = alf)
    res2 <- composite_pair_loss(x1[, 2], fun = alf)
    res3 <- composite_pair_loss(x1[, 3], fun = alf)
    expect_true(res3 > res2 & res2 > res1)
})

test_that("par_to_mat works correctly", {
    pt <- data.frame(
        lhs = c(
            "f1",
            "f1",
            "f1",
            "f2",
            "f2",
            "f2",
            "f3",
            "f3",
            "f3",
            "f3",
            "y1",
            "y2",
            "y3",
            "y4",
            "y5",
            "y6",
            "y7",
            "y8",
            "y9",
            "y10",
            "f1",
            "f2",
            "f3",
            "f1",
            "f2",
            "f3"
        ),
        op = c(
            "=~",
            "=~",
            "=~",
            "=~",
            "=~",
            "=~",
            "=~",
            "=~",
            "=~",
            "=~",
            "~1",
            "~1",
            "~1",
            "~1",
            "~1",
            "~1",
            "~1",
            "~1",
            "~1",
            "~1",
            "~1",
            "~1",
            "~1",
            "~~",
            "~~",
            "~~"
        ),
        rhs = c(
            "y1",
            "y2",
            "y3",
            "y4",
            "y5",
            "y6",
            "y7",
            "y8",
            "y9",
            "y10",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "f1",
            "f2",
            "f3"
        ),
        free = c(4:6, 1:3, 7:10, 13:22, rep(0, 4), 11:12),
        stringsAsFactors = FALSE
    )
    ind_mat <- matrix(
        c(paste0("y", 1:3), NA, paste0("y", 4:6), NA, paste0("y", 7:10)),
        ncol = 3
    )

    ld_mat <- par_to_mat(1:24, op = "=~", pt = pt, ind_matrix = ind_mat)
    int_mat <- par_to_mat(1:24, op = "~1", pt = pt, ind_matrix = ind_mat)

    expect_equal(ld_mat, matrix(c(4:6, NA, 1:3, NA, 7:10), ncol = 3))
    expect_equal(int_mat, matrix(c(13:15, NA, 16:18, NA, 19:22), ncol = 3))
})
