# Adapted from lavaan:::lav_dataframe_vartable()
get_nlev_data <- function(
    data = NULL,
    ordered = NULL,
    allow.empty.cell = FALSE
) {
    nvar <- ncol(data)
    nlev <- integer(nvar)
    varnames <- colnames(data)
    names(nlev) <- varnames
    if (isTRUE(ordered)) {
        ordered <- varnames
    }
    for (j in intersect(ordered, varnames)) {
        nlev[[j]] <- get_nlev_ordered(data[, j], allow.empty.cell)
    }
    nlev
}

get_nlev_ordered <- function(
    x,
    allow.empty.cell = FALSE
) {
    if (allow.empty.cell) {
        if (inherits(x, "factor")) {
            return(nlevels(x))
        } else {
            return(max(as.numeric(x), na.rm = TRUE))
        }
    } else {
        return(length(unique(x[!is.na(x)])))
    }
}

partial_string_to_list <- function(x, ind_matrix) {
    out <- list()
    for (xi in x) {
        if (grepl("=~", xi)) {
            type_xi <- "loadings"
            vi <- strsplit(xi, "=~")[[1]][2]
        } else if (grepl("~1", xi)) {
            type_xi <- "intercepts"
            vi <- strsplit(xi, "~1")[[1]][1]
        } else if (grepl("~~", xi)) {
            type_xi <- "residuals"
            vi <- strsplit(xi, "~~")[[1]][1]
        } else {
            stop("Unrecognized operator in ", xi)
        }
        vi <- trimws(vi)
        if (!vi %in% ind_matrix) {
            stop(vi, " is not part of `ind_matrix`.")
        }
        row_i <- which(
            ind_matrix == vi,
            arr.ind = TRUE,
            useNames = FALSE
        )
        if (is.null(out[[type_xi]])) {
            out[[type_xi]] <- list(row_i)
        } else {
            out[[type_xi]][[length(out[[type_xi]]) + 1L]] <- row_i
        }
    }
    lapply(out, do.call, what = "rbind")
}

pt_to_partial_string <- function(pt) {
    if (is.null(pt) || nrow(pt) == 0) return(character(0))
    paste(pt$lhs, pt$op, pt$rhs)
}
