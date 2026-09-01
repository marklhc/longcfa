library(lavaan)

test_that("penalized_longcfa() threads `test` through to penalized_est()", {
    skip_if_not_installed("plavaan")

    # Fit evaluation (the `test` argument and effective_df()) requires a
    # recent plavaan; skip cleanly on older builds.
    has_fit_eval <-
        "test" %in% names(formals(plavaan::penalized_est)) &&
            exists("effective_df", envir = asNamespace("plavaan"))
    skip_if_not(
        has_fit_eval,
        "plavaan build lacks fit evaluation (test argument / effective_df())"
    )

    data("PoliticalDemocracy", package = "lavaan")
    ind_mat <- cbind(c("y1", "y2", "y3", "y4"), c("y5", "y6", "y7", "y8"))
    lv <- c("dem60", "dem65")

    # Default test = "none": fit evaluation is disabled
    pen_none <- suppressWarnings(penalized_longcfa(
        ind_matrix = ind_mat,
        lv_names = lv,
        data = PoliticalDemocracy,
        w = 0.1
    ))
    expect_equal(attr(pen_none, "penalized")$test, "none")
    expect_null(suppressMessages(fitmeasures(pen_none, "cfi")))

    # test = "Chisq": fit measures are available at the effective df
    pen_chisq <- suppressWarnings(penalized_longcfa(
        ind_matrix = ind_mat,
        lv_names = lv,
        data = PoliticalDemocracy,
        w = 0.1,
        test = "Chisq"
    ))
    expect_equal(attr(pen_chisq, "penalized")$test, "Chisq")

    # fitmeasures() returns a named vector of measures (at the effective df)
    fm <- suppressMessages(fitmeasures(pen_chisq))
    if (is.matrix(fm)) {
        fm <- setNames(as.numeric(fm[1, ]), colnames(fm))
    }
    req <- c("chisq", "df", "cfi", "rmsea", "srmr")
    expect_true(all(req %in% names(fm)))
    vals <- unname(as.numeric(fm[req]))
    expect_true(all(is.finite(vals)))
    expect_gt(as.numeric(fm["df"]), 0)

    # The reported df is the (penalty-adjusted) effective df
    efdf_info <- attr(plavaan::effective_df(pen_chisq), "info")
    expect_equal(
        as.numeric(fm["df"]),
        efdf_info$df_model_effective,
        tolerance = 1e-2
    )
})
