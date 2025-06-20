test_that("continuous configural with NA", {
    syn1 <- longcfa_syntax(
        ind_mat = ind_mat1,
        lv_names = c("G1", "G4", "G7")
    )
    expect_match(
        syn1,
        "G1 ~ 0 * 1
G4 ~ 0 * 1
G7 ~ 0 * 1",
        fixed = TRUE
    )
    expect_match(
        syn1,
        "G1 ~~ 1 * G1
G4 ~~ 1 * G4
G7 ~~ 1 * G7",
        fixed = TRUE
    )
})

test_that("relax variances with metric", {
    syn2 <- longcfa_syntax(
        ind_mat = ind_mat1,
        lv_names = c("G1", "G4", "G7"),
        long_equal = "loadings"
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
