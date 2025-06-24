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
        return(length(sort(unique(x))))
    }
}
