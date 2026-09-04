library(longcfa)
library(lavaan)
data("PoliticalDemocracy", package = "lavaan")
filter_pt <- longcfa:::filter_pt
filter_cons <- longcfa:::filter_cons
next_to_relax <- longcfa:::next_to_relax
lav_constraints_rm <- longcfa:::lav_constraints_rm
get_op_idx <- longcfa:::get_op_idx
get_lav_lrt <- longcfa:::get_lav_lrt
count_tied_inds <- longcfa:::count_tied_inds
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
        c("a", "b", "c", "d", "e", "f"),
        function(i) nrow(lav_constraints_rm(pt1, plab = i)),
        FUN.VALUE = integer(1)
    )
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

test_that("par_to_mat_from_pt works correctly for thresholds", {
    par_to_mat_from_pt <- longcfa:::par_to_mat_from_pt

    pt <- data.frame(
        lhs = c("y1", "y2", "y3", "y4", "y1", "y2", "y3", "y4"),
        op = rep("|", 8),
        rhs = c("th1", "th1", "th1", "th1", "th2", "th2", "th2", "th2"),
        free = 1:8,
        stringsAsFactors = FALSE
    )

    ind_mat <- matrix(c("y1", "y2", "y3", "y4"), ncol = 2)

    out <- par_to_mat_from_pt(
        pt,
        op = "|",
        ind_matrix = ind_mat,
        out_col = "free"
    )

    expect_equal(out, matrix(c(1, 2, 5, 6, 3, 4, 7, 8), ncol = 2))
})

test_that("get_lav_lrt works", {
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
    # one LRT row per equality constraint, at its `==` rhs endpoint
    # (the wave-2 parameter)
    lrt1 <- get_lav_lrt(fit1, ind = c(ind_mat), op = "~1")
    expect_equal(nrow(lrt1), 4)
    expect_equal(lrt1$lhs, c("y5", "y6", "y7", "y8"))
    expect_equal(lrt1$plabel, c(".p17.", ".p18.", ".p19.", ".p20."))
    expect_equal(
        lrt1$mi,
        c(0.3812392, 5.9803683, 0.4066102, 1.0359694),
        tolerance = 1e-5
    )
    expect_equal(
        lrt1$p,
        c(0.53694079, 0.01446598, 0.52369505, 0.30876073),
        tolerance = 1e-5
    )
    lrt2 <- get_lav_lrt(fit1, ind = c(ind_mat), op = "=~")
    expect_equal(nrow(lrt2), 4)
    expect_equal(lrt2$rhs, c("y5", "y6", "y7", "y8"))
    expect_equal(lrt2$plabel, c(".p13.", ".p14.", ".p15.", ".p16."))
    expect_equal(
        lrt2$mi,
        c(0.62783371, 0.31837862, 2.88991566, 0.04811103),
        tolerance = 1e-5
    )
    expect_equal(
        lrt2$p,
        c(0.42815104, 0.57258367, 0.08913559, 0.82638344),
        tolerance = 1e-5
    )
    # a configural fit has no tied constraints: empty 8-column frame
    fit_c <- longcfa(
        ind_mat,
        lv_names = c("dem60", "dem65"),
        data = PoliticalDemocracy,
        lag_cov = TRUE,
        long_equal = NULL
    )
    lrt_c <- get_lav_lrt(fit_c, ind = c(ind_mat), op = "~1")
    expect_equal(nrow(lrt_c), 0)
    expect_equal(
        colnames(lrt_c),
        c("id", "lhs", "op", "rhs", "group", "plabel", "mi", "p")
    )
})

test_that("get_lav_lrt reports the in-`ind` endpoint when `ind` is a subset", {
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
    # wave-1 only: the `==` row's rhs is the (excluded) wave-2 plabel, so the
    # reported candidate must fall back to the wave-1 endpoint, not NA
    lrt_w1 <- get_lav_lrt(fit1, ind = c("y1", "y2", "y3", "y4"), op = "=~")
    expect_equal(nrow(lrt_w1), 4)
    expect_equal(lrt_w1$rhs, c("y1", "y2", "y3", "y4"))
    expect_false(anyNA(lrt_w1$plabel))
    expect_false(anyNA(lrt_w1$id))
    # restricting to wave-2 keeps the original rhs-based reporting
    lrt_w2 <- get_lav_lrt(fit1, ind = c("y5", "y6", "y7", "y8"), op = "=~")
    expect_equal(lrt_w2$rhs, c("y5", "y6", "y7", "y8"))
    expect_false(anyNA(lrt_w2$plabel))
})

test_that("get_lav_mod returns a p column", {
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
    m <- get_lav_mod(fit1, ind = c(ind_mat), op = "~1")
    expect_true("p" %in% names(m))
    # lavaan 0.7+ no longer reports p from modindices(); each mi is a
    # 1-df chi-square test, so p must equal pchisq(mi, 1, lower.tail = FALSE)
    expect_equal(m$p, pchisq(m$mi, 1, lower.tail = FALSE))
})

test_that("plinv_search records mi and p in traces", {
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
        mi_min = 2.00,
        lag_cov = TRUE
    )
    expect_equal(ps1$traces$rhs[1], "y7")
    expect_equal(ps1$traces$lhs[2], "y6")
    expect_equal(
        ps1$traces$mi[c(1, 2)], c(2.983904, 5.375750),
        tolerance = 1e-5
    )
    expect_equal(
        ps1$traces$p[c(1, 2)], c(0.08409623, 0.02041856),
        tolerance = 1e-5
    )
})

test_that("plinv_search with control_fdr", {
    ind_mat <- matrix(
        c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"),
        nrow = 4
    )
    # With get_lav_test_score each stage reports n_cand = 4 tied
    # candidates, so the first (strictest) cutoff is
    # qchisq(pinsearch::fdr_alpha(1, 4, q), 1, lower.tail = FALSE)
    # = 6.2605 at sig_level = 0.05, above the stage maxima (2.98 for
    # loadings, 5.82 for intercepts), so nothing is freed.
    ps1 <- plinv_search(
        ind_mat,
        lv_names = c("dem60", "dem65"),
        data = PoliticalDemocracy,
        type = c("loadings", "intercepts"),
        mi_fun = get_lav_test_score,
        mi_min = 2.00,
        control_fdr = TRUE,
        sig_level = 0.05,
        lag_cov = TRUE
    )
    expect_null(ps1$traces)
    # a lenient sig_level frees the same parameters as the mi_min path
    ps2 <- plinv_search(
        ind_mat,
        lv_names = c("dem60", "dem65"),
        data = PoliticalDemocracy,
        type = c("loadings", "intercepts"),
        mi_fun = get_lav_test_score,
        mi_min = 2.00,
        control_fdr = TRUE,
        sig_level = 0.5,
        lag_cov = TRUE
    )
    expect_equal(ps2$traces$rhs[1], "y7")
    expect_equal(ps2$traces$lhs[2], "y6")
    expect_equal(
        ps2$traces$mi[c(1, 2)], c(2.983904, 5.375750),
        tolerance = 1e-5
    )
    expect_equal(
        ps2$traces$p[c(1, 2)], c(0.08409623, 0.02041856),
        tolerance = 1e-5
    )
})

test_that("count_tied_inds counts items still tied", {
    # Minimal partable-like frame mirroring the real layout: a `==` row
    # carries the second occurrence's plabel on its rhs (lhs is the first
    # occurrence's plabel), with group 0, free 0, and no plabel of its own.
    pt <- data.frame(
        id = 1:9,
        lhs = c(
            "dem60", "dem60", "dem65", "dem65",
            "y1", "y5",
            ".p1.", ".p2.", ".p5."
        ),
        op = c("=~", "=~", "=~", "=~", "~1", "~1", "==", "==", "=="),
        rhs = c(
            "y1", "y2", "y5", "y6", "", "",
            ".p13.", ".p14.", ".p17."
        ),
        group = c(1, 1, 1, 1, 1, 1, 0, 0, 0),
        free = c(1, 2, 3, 4, 5, 6, 0, 0, 0),
        plabel = c(".p1.", ".p2.", ".p13.", ".p14.", ".p5.", ".p17.",
                   "", "", ""),
        label = rep("", 9),
        stringsAsFactors = FALSE
    )
    ind_mini <- matrix(c("y1", "y2", "y5", "y6"), ncol = 2)
    # both items are tied on loadings
    expect_equal(count_tied_inds(pt, "=~", ind_mini), 2)
    # only item (y1, y5) is tied on intercepts; "~1" reads the lhs column
    expect_equal(count_tied_inds(pt, "~1", ind_mini), 1)
    # free = 0 rows are excluded: fixing y1's loading leaves item
    # (y1, y5) tied through y5 alone, so the count is unchanged
    pt1 <- pt
    pt1$free[1] <- 0
    expect_equal(count_tied_inds(pt1, "=~", ind_mini), 2)
    # ... and freeing y5's loading as well unties the item
    pt2 <- pt1
    pt2$free[3] <- 0
    expect_equal(count_tied_inds(pt2, "=~", ind_mini), 1)
    # with y2/y6's pair removed and y5's loading freed, only y1 keeps a
    # plabel in a `==` row, so a single item is still tied
    pt3 <- pt[-8, ]
    pt3$free[3] <- 0
    expect_equal(count_tied_inds(pt3, "=~", ind_mini), 1)
    # NA cells are ignored; the item is counted by its present variable
    ind_na <- matrix(c("y1", NA, "y2", "y6"), ncol = 2)
    expect_equal(count_tied_inds(pt, "=~", ind_na), 2)
    # a row with only NA (no variable present) is never "tied": %in% maps an
    # NA needle to FALSE (not NA), so any(...) is FALSE and the count is 0,
    # keeping the integer result that the min2 guard relies on
    ind_allna <- matrix(NA, nrow = 2, ncol = 2)
    expect_equal(count_tied_inds(pt, "=~", ind_allna), 0)
    expect_true(is.integer(count_tied_inds(pt, "=~", ind_allna)))
    # an all-NA row alongside a tied row counts only the tied one
    ind_mix <- rbind(c("y1", "y5"), c(NA, NA))
    expect_equal(count_tied_inds(pt, "=~", ind_mix), 1)
    # variables absent from the partable are not tied and do not error
    ind_absent <- matrix(c("zx", "zy", "aa", "bb"), ncol = 2)
    expect_equal(count_tied_inds(pt, "=~", ind_absent), 0)
})

test_that("plinv_search with min2 keeps two tied items", {
    ind_mat <- matrix(
        c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"),
        nrow = 4
    )
    # fake mi_fun: an 8-column frame with mi forced to 1e10, one row per
    # candidate still tied for this op (as selected by get_lav_mod). It is
    # built from partable() rather than get_lav_mod() because once the last
    # equality pair of the stage is released that wave is no longer locally
    # identified, and modindices() would error on the refit in that case.
    fake <- function(x, op, ind) {
        pt <- partable(x)
        ind_col <- if (op == "=~") "rhs" else "lhs"
        pt_op <- pt[pt$op == op & pt[[ind_col]] %in% ind & pt$free > 0, ]
        eq_plab <- unlist(pt[pt$op == "==", c("lhs", "rhs")],
                          use.names = FALSE)
        pt_op <- pt_op[
            pt_op$plabel %in% eq_plab,
            c("id", "lhs", "op", "rhs", "group", "plabel")
        ]
        cbind(pt_op, mi = rep(1e10, nrow(pt_op)), p = rep(0, nrow(pt_op)))
    }
    psm <- plinv_search(
        ind_mat,
        lv_names = c("dem60", "dem65"),
        data = PoliticalDemocracy,
        type = "loadings",
        mi_fun = fake,
        mi_min = 0,
        min2 = TRUE,
        lag_cov = TRUE
    )
    # the stage stops with two items (y3, y4) still tied
    expect_equal(nrow(psm$traces), 2)
    expect_equal(psm$traces$rhs, c("y1", "y2"))
    expect_equal(count_tied_inds(partable(psm$fit), "=~", ind_mat), 2)
    psm2 <- plinv_search(
        ind_mat,
        lv_names = c("dem60", "dem65"),
        data = PoliticalDemocracy,
        type = "loadings",
        mi_fun = fake,
        mi_min = 0,
        min2 = FALSE,
        lag_cov = TRUE
    )
    # without the guard all four equality pairs are released
    expect_equal(nrow(psm2$traces), 4)
    expect_equal(psm2$traces$rhs, c("y1", "y2", "y3", "y4"))
    expect_equal(count_tied_inds(partable(psm2$fit), "=~", ind_mat), 0)
})
