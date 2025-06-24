test_that("continuous configural with NA", {
    syn1 <- longcfa_syntax(
        ind_mat = ind_mat1,
        lv_names = c("G1", "G4", "G7")
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

test_that("relax variances with metric", {
    syn2 <- longcfa_syntax(
        ind_mat = ind_mat1,
        lv_names = c("G1", "G4", "G7"),
        long_equal = "loadings"
    )
    pt2 <- lavaan::lavaanify(syn2)
    expect_true(
        all(
            pt2[
                pt2$rhs %in% c("cog1", "cog4", "cog7") & pt2$op == "=~",
                "label"
            ] ==
                ".l1"
        )
    )
    expect_true(
        anyDuplicated(
            pt2[
                pt2$lhs %in% c("cog1", "cog4", "cog7") & pt2$op == "~1",
                "label"
            ]
        ) ==
            0 |
            length(unique(
                pt2[
                    pt2$lhs %in% c("cog1", "cog4", "cog7") & pt2$op == "~1",
                    "label"
                ]
            )) ==
                1
    )
    expect_match(
        syn2,
        ".l2 * verb7",
        fixed = TRUE
    )
    expect_no_match(
        syn2,
        "G4 ~ 1 * G1"
    )
})

test_that("relax means with scalar", {
    syn3 <- longcfa_syntax(
        ind_mat = ind_mat1,
        lv_names = c("G1", "G4", "G7"),
        long_equal = c("loadings", "intercepts")
    )
    pt3 <- lavaan::lavaanify(syn3)
    expect_true(
        all(
            pt3[
                pt3$lhs %in% c("verb1", "verb4", "verb7") & pt3$op == "~1",
                "label"
            ] ==
                ".i2"
        )
    )
    expect_true(
        anyDuplicated(
            pt3[
                pt3$lhs %in% c("write1", "write4", "write7") & pt3$op == "~~",
                "label"
            ]
        ) ==
            0 |
            length(unique(
                pt3[
                    pt3$lhs %in%
                        c("write1", "write4", "write7") &
                        pt3$op == "~~",
                    "label"
                ]
            )) ==
                1
    )
    expect_match(
        syn3,
        "write4 ~ .i3 * 1",
        fixed = TRUE
    )
    expect_no_match(
        syn3,
        "G4 ~ 0 * 1",
        fixed = TRUE
    )
    expect_no_match(
        syn3,
        "G7 ~~ 1 * G7",
        fixed = TRUE
    )
})

test_that("partial metric", {
    expect_error(
        longcfa_syntax(
            ind_mat = ind_mat1,
            lv_names = c("G1", "G4", "G7"),
            long_equal = c("loadings"),
            long_partial = matrix(c(3, 3), nrow = 1)
        ),
        "`long_partial` must be a named list"
    )
    syn4 <- longcfa_syntax(
        ind_mat = ind_mat1,
        lv_names = c("G1", "G4", "G7"),
        long_equal = c("loadings"),
        long_partial = list(
            loadings = matrix(c(3, 2), nrow = 1)
        )
    )
    pt4 <- lavaan::lavaanify(syn4)
    expect_equal(
        pt4[pt4$rhs == "write4" & pt4$op == "=~", "label"],
        ".l3_2"
    )
    expect_no_match(
        syn4,
        "G7 ~~ 1 * G7",
        fixed = TRUE
    )
    expect_match(
        syn4,
        "G4 ~ 0 * 1",
        fixed = TRUE
    )
})

test_that("partial scalar", {
    syn5 <- longcfa_syntax(
        ind_mat = ind_mat1,
        lv_names = c("G1", "G4", "G7"),
        long_equal = c("intercepts"),
        long_partial = list(
            intercepts = matrix(c(1, 2), nrow = 1)
        )
    )
    pt5 <- lavaan::lavaanify(syn5)
    expect_equal(
        pt5[pt5$lhs == "cog4" & pt5$op == "~1", "label"],
        ".i1_2"
    )
    expect_match(
        syn5,
        "G4 ~~ 1 * G4",
        fixed = TRUE
    )
    expect_no_match(
        syn5,
        "G7 ~ 0 * 1",
        fixed = TRUE
    )
})

test_that("strict invariance", {
    syn6 <- longcfa_syntax(
        ind_mat = ind_mat1,
        lv_names = c("G1", "G4", "G7"),
        long_equal = c("loadings", "intercepts", "residuals")
    )
    pt6 <- lavaan::lavaanify(syn6)
    expect_true(
        all(
            pt6[
                pt6$lhs %in% c("write1", "write4", "write7") & pt6$op == "~~",
                "label"
            ] ==
                ".u3"
        )
    )
    expect_no_match(
        syn6,
        "G4 ~~ 1 * G4",
        fixed = TRUE
    )
    expect_no_match(
        syn6,
        "G7 ~ 0 * 1",
        fixed = TRUE
    )
})

# test_that("strict invariance with covariances", {
#     syn7 <- longcfa_syntax(
#         ind_mat = ind_mat1,
#         lv_names = c("G1", "G4", "G7"),
#         long_equal = c("loadings", "intercepts", "residuals"),
#         lag_cov = TRUE
#     )
# })

test_that("convert syntax to partial string", {
    expect_equal(
        partial_string_to_list(
            c("eta1 =~ cog1", "verb7 ~~ verb7", "write4~1"),
            ind_mat1
        ),
        list(
            loadings = matrix(c(1, 1), nrow = 1),
            residuals = matrix(c(2, 3), nrow = 1),
            intercepts = matrix(c(3, 2), nrow = 1)
        )
    )
    expect_error(
        partial_string_to_list(
            c("eta1 =~ cog1", "verb8 ~~ verb8"),
            ind_mat1
        ),
        "not part of `ind_matrix`."
    )
    expect_error(
        partial_string_to_list(
            c("eta1 ~ cog1", "verb8 ~~ verb8"),
            ind_mat1
        ),
        "Unrecognized operator"
    )
})
