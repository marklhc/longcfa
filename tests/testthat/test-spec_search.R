library(lavaan)

test_that("Specification search works", {
    ind_mat <- matrix(
        c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"),
        nrow = 4
    )
    fit1 <- longcfa(
        ind_mat,
        lv_names = c("dem60", "dem65"),
        data = PoliticalDemocracy,
        lag_cov = TRUE,
        long_equal = c("loadings", "intercepts")
    )
    pt1 <- partable(fit1)
    fil1 <- filter_pt(pt1, ind = c(ind_mat), op = "~~")
    expect_equal(nrow(fil1), 12)
    cons1 <- filter_cons(fil1, pt1[pt1$op == "==", ])
    expect_length(cons1, 0)
    fil2 <- filter_pt(pt1, ind = c(ind_mat), op = "~1")
    expect_equal(nrow(fil2), 8)
    cons2 <- filter_cons(fil2, pt1[pt1$op == "==", ])
    expect_equal(cons2, 5:8)
    score2 <- get_lav_test_score(fit1, ind = c(ind_mat), op = "~1")
    expect_equal(which.max(score2$mi), 2)
    next_to_relax(
        fit1,
        get_lav_test_score,
        fn_min = 3.84,
        ind = c(ind_mat),
        op = "~1"
    )
})

test_that("lav_constraints_rm works", {
    pt1 <- data.frame(
        id = 1:6,
        lhs = c("a", "a", "d", "c", "d", "g"),
        op = rep("==", 6),
        rhs = c("b", "c", "b", "e", "f", "h")
    )
    pts_new <- vapply(
        c("a", "b", "c", "d", "e", "f"),
        \(i) nrow(lav_constraints_rm(pt1, plab = i)),
        FUN.VALUE = integer(1)
    )
    expect_true(all(pts_new == 5))
})
