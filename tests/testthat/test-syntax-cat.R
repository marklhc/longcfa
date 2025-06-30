test_that("categorical configural with NA", {
    syn1 <- longcfa_syntax(
        ind_mat = ind_mat1,
        lv_names = c("G1", "G4", "G7"),
        nthres = matrix(c(2, 3, 3), nrow = 3, ncol = 3)
    )
    pt1 <- lavaan::lavaanify(syn1)
    pt1_lmean <- subset(pt1, lhs %in% c("G1", "G4", "G7") & op == "~1")
    expect_equal(pt1_lmean$free, rep(0, 3))
    expect_equal(pt1_lmean$ustart, rep(0, 3))
    pt1_lvar <- subset(
        pt1,
        lhs %in% c("G1", "G4", "G7") & op == "~~" & rhs == lhs
    )
    expect_equal(pt1_lvar$free, rep(0, 3))
    expect_equal(pt1_lvar$ustart, rep(1, 3))
})
