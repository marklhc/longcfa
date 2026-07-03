library(longcfa)
library(lavaan)
data("PoliticalDemocracy", package = "lavaan")
filter_pt <- longcfa:::filter_pt; filter_cons <- longcfa:::filter_cons; next_to_relax <- longcfa:::next_to_relax
lav_constraints_rm <- longcfa:::lav_constraints_rm; get_op_idx <- longcfa:::get_op_idx
test_that("Extract score test works", {
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
    to_free <- next_to_relax(
        fit1,
        get_lav_test_score,
        fn_min = 3.84,
        ind = c(ind_mat),
        op = "~1"
    )
    to_free2 <- next_to_relax(
        fit1,
        get_lav_mod,
        fn_min = 3.84,
        ind = c(ind_mat),
        op = "~1"
    )
    expect_equal(to_free$lhs, "y6")
    expect_true(to_free2$lhs %in% c("y2", "y6"))
})

test_that("lav_constraints_rm works", {
    pt1 <- data.frame(
        id = 1:6,
        lhs = c("a", "a", "d", "c", "d", "g"),
        op = rep("==", 6),
        rhs = c("b", "c", "b", "e", "f", "h")
    )
    pts_new <- vapply(
        function(i) nrow(lav_constraints_rm(pt1, plab = i)),
        FUN.VALUE = integer(1)
    expect_true(all(pts_new == 5))
})

test_that("Specification search works", {
    ind_mat <- matrix(
        c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"),
        nrow = 4
    )
    ps1 <- plinv_search(
        ind_mat,
        lv_names = c("dem60", "dem65"),
        data = PoliticalDemocracy,
        type = c("loadings", "intercepts"),
        mi_fun = get_lav_test_score,
        mi_min = 2.00
    )
    expect_equal(ps1$traces$rhs[1], "y7")
    expect_equal(ps1$traces$lhs[2], "y6")
    skip()
    ind_mat <- matrix(
        grep("^ssa", names(mackinnon_etal_wide), value = TRUE),
        nrow = 7,
        byrow = TRUE
    )
    mackinnon_etal_wide[-1] <- lapply(mackinnon_etal_wide[-1], as.integer)
    fit_s <- longcfa(
        ind_mat,
        lv_names = paste0("SSA", 2:8),
        data = mackinnon_etal_wide,
        lag_cov = TRUE,
        long_equal = c("loadings", "intercepts")
    )
    ps1 <- plinv_search_step(
        fit_s,
        mi_fun = get_lav_test_score,
        mi_min = 10,
        op = "~1",
        ind = c(ind_mat)
    )
    ps2 <- plinv_search_step(
        fit_s,
        mi_fun = get_lav_mod,
        mi_min = 5.99,
        op = "~1",
        ind = c(ind_mat)
    )
    expect_equal(ps1$trace$lhs, "ssa7_2")
    expect_equal(ps2$trace$lhs, "ssa7_2")
})

test_that("get_op_idx works", {
    ind_mat <- matrix(
        c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"),
        nrow = 4
    )
    fit1 <- cfa(
        "
      dem60 =~ y1 + y2 + y3 + y4
      dem65 =~ y5 + y6 + y7 + y8
      ",
        data = PoliticalDemocracy,
        std.lv = FALSE,
        meanstructure = TRUE
    )
    load_ids <- get_lav_par_id(fit1, op = "=~", ind_matrix = ind_mat)
    int_ids <- get_lav_par_id(fit1, op = "~1", ind_matrix = ind_mat)
    expect_equal(load_ids, matrix(c(0, 1:3, 0, 4:6), nrow = 4))
    expect_equal(int_ids, matrix(18:25, nrow = 4))
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

    ld_mat <- get_op_idx(pt, "=~", ind_mat)
    int_mat <- get_op_idx(pt, "~1", ind_mat)

    expect_equal(ld_mat, c(4:6, NA, 1:3, NA, 7:10))
    expect_equal(int_mat, c(13:15, NA, 16:18, NA, 19:22))
})
