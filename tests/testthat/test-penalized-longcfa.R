library(lavaan)

# penalized_est_multistart() was added in plavaan 0.0.2; helper to branch tests.
plavaan_has_multistart <- function() {
    requireNamespace("plavaan", quietly = TRUE) &&
        "penalized_est_multistart" %in% getNamespaceExports("plavaan")
}

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

test_that("penalized_longcfa() validates the `test` argument", {
    skip_if_not_installed("plavaan")
    data("PoliticalDemocracy", package = "lavaan")
    ind_mat <- cbind(c("y1", "y2", "y3", "y4"), c("y5", "y6", "y7", "y8"))
    lv <- c("dem60", "dem65")

    for (bad in list(NULL, NA, c("none", "Chisq"), 123)) {
        expect_error(
            penalized_longcfa(ind_mat, lv, PoliticalDemocracy, w = 0.1, test = bad),
            "single string"
        )
    }
})

test_that("penalized_longcfa() routes plavaan_args to the plavaan estimator", {
    skip_if_not_installed("plavaan")
    data("PoliticalDemocracy", package = "lavaan")
    ind_mat <- cbind(c("y1", "y2", "y3", "y4"), c("y5", "y6", "y7", "y8"))
    lv <- c("dem60", "dem65")

    # default: single start, no multistart attribute
    single <- suppressWarnings(
        penalized_longcfa(ind_mat, lv, PoliticalDemocracy, w = 0.1)
    )
    expect_null(attr(single, "multistart"))

    # n_starts > 1 switches to penalized_est_multistart() when available;
    # otherwise requesting it fails with a clear message
    if (plavaan_has_multistart()) {
        set.seed(1)
        ms <- suppressWarnings(
            penalized_longcfa(
                ind_mat, lv, PoliticalDemocracy, w = 0.1,
                plavaan_args = list(n_starts = 2)
            )
        )
        ms_tab <- attr(ms, "multistart")
        expect_true(is.data.frame(ms_tab))
        expect_equal(nrow(ms_tab), 2L)
        expect_true(all(c("start_id", "objective", "converged") %in% names(ms_tab)))
    } else {
        expect_error(
            penalized_longcfa(
                ind_mat, lv, PoliticalDemocracy, w = 0.1,
                plavaan_args = list(n_starts = 2)
            ),
            "penalized_est_multistart"
        )
    }

    # custom single start via `start`
    fit_un <- longcfa(
        ind_mat, lv_names = lv, data = PoliticalDemocracy,
        free_latvars = TRUE, free_latmeans = TRUE, do.fit = FALSE
    )
    sv <- lavaan::lav_export_estimation(fit_un)$starting_values
    st <- suppressWarnings(
        penalized_longcfa(
            ind_mat, lv, PoliticalDemocracy, w = 0.1,
            plavaan_args = list(start = sv)
        )
    )
    expect_true(inherits(st, "lavaan"))

    # an option the installed plavaan lacks -> clean "does not support" error
    expect_error(
        penalized_longcfa(
            ind_mat, lv, PoliticalDemocracy, w = 0.1,
            plavaan_args = list(bogus_option = 1)
        ),
        "does not support"
    )

    # core options must be set via their dedicated args, not plavaan_args
    for (core in list(
        list(w = 0.5),
        list(se = "robust.huber.white"),
        list(test = "Chisq")
    )) {
        expect_error(
            penalized_longcfa(ind_mat, lv, PoliticalDemocracy, w = 0.1, plavaan_args = core),
            "dedicated arguments"
        )
    }

    # n_starts <= 1 (or NA) is a single start: n_starts/starts are dropped and
    # no multistart table is attached
    for (nsv in list(1, NA)) {
        one <- suppressWarnings(
            penalized_longcfa(
                ind_mat, lv, PoliticalDemocracy, w = 0.1,
                plavaan_args = list(n_starts = nsv)
            )
        )
        expect_null(attr(one, "multistart"))
    }
})

test_that("penalized_longcfa() forwards eps/telescoping via plavaan_args (when supported)", {
    skip_if_not_installed("plavaan")
    if (!"eps" %in% names(formals(plavaan::penalized_est))) {
        skip("plavaan build lacks `eps` support")
    }
    data("PoliticalDemocracy", package = "lavaan")
    ind_mat <- cbind(c("y1", "y2", "y3", "y4"), c("y5", "y6", "y7", "y8"))
    lv <- c("dem60", "dem65")

    tl <- suppressWarnings(
        penalized_longcfa(
            ind_mat, lv, PoliticalDemocracy, w = 0.1,
            plavaan_args = list(
                eps = "telescoping",
                telescoping_control = list(eps_steps = 4)
            )
        )
    )
    expect_true(is.data.frame(attr(tl, "telescoping")))
})

test_that("penalized_longcfa() rejects `test` together with multistart", {
    skip_if_not_installed("plavaan")
    data("PoliticalDemocracy", package = "lavaan")
    ind_mat <- cbind(c("y1", "y2", "y3", "y4"), c("y5", "y6", "y7", "y8"))
    lv <- c("dem60", "dem65")

    # with multistart available: the fit-measures (test) combination is rejected
    # without multistart: the availability error is raised first
    if (plavaan_has_multistart()) {
        expect_error(
            penalized_longcfa(
                ind_mat, lv, PoliticalDemocracy, w = 0.1,
                test = "Chisq",
                plavaan_args = list(n_starts = 2)
            ),
            "multistart"
        )
    } else {
        expect_error(
            penalized_longcfa(
                ind_mat, lv, PoliticalDemocracy, w = 0.1,
                test = "Chisq",
                plavaan_args = list(n_starts = 2)
            ),
            "penalized_est_multistart"
        )
    }
})

test_that("penalized_longcfa() validates the plavaan_args type", {
    skip_if_not_installed("plavaan")
    data("PoliticalDemocracy", package = "lavaan")
    ind_mat <- cbind(c("y1", "y2", "y3", "y4"), c("y5", "y6", "y7", "y8"))
    lv <- c("dem60", "dem65")

    for (bad in list("notalist", 1, list(1, 2), list(n_starts = 2, 3))) {
        expect_error(
            penalized_longcfa(ind_mat, lv, PoliticalDemocracy, w = 0.1, plavaan_args = bad),
            "named list"
        )
    }
})
