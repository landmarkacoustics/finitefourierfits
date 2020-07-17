## Copyright (C) 2020 by Landmark Acoustics LLC

#' Fit a small, Fourier basis-like model to data
#'
#' When a functional-looking relation is poorly approximated by the tools `lm`,
#' `glm`, or `nls`, one option is to use a linear combination of cosine terms
#' to approximate the relation within a strictly bounded domain. This function
#' does just that.
#'
#' @param x Numeric, the independent variable
#' @param y Numeric, the dependent variable
#' @param model.selector Function, optional, the function for comparing models
#' @param max.terms Integer, optional, the greatest number of terms allowed
#' @return an FFFit model
#' @examples
#' x <- rnorm(10)
#' y <- 10*x + rnorm(10)
#' my.fit <- ff.fit(x, y)
#' @export
#' @importFrom stats BIC fft nextn nls
ff.fit <- function(x, y, model.selector=BIC, max.terms=10) {

    N <- length(x)
    if (length(y) != N) {
        stop("`x` and `y` must be the same length.")
    }

    fft.size <- 4*nextn(N, 2)

    u <- .domain.shift(fft.size, x)

    S <- fft(c(y, rep(0, fft.size - N)))

    a <- .amplitudes(S)
    p <- .phases(S)

    magnitude.order <- order(a, decreasing=TRUE)[1:max.terms]

    lhs <- .somehow.get.the.name.of.y(y)

    term.list <- build.term.list(magnitude.order, p)
    start.list <- as.list(a[magnitude.order])
    names(start.list) <- names(term.list)

    models <- list()
    for (i in 1:max.terms) {
        tmp <- tryCatch(.build.model(lhs,
                                     term.list[1:i],
                                     data.frame(w=u(x),
                                                y=y),
                                     start.list[1:i]),
                        error=.null.on.error)
        if (!is.null(tmp)) {
            models <- append(models, list(tmp))
        }
    }
    best.model.index <- .argmin(sapply(models, model.selector))
    return(list(response=lhs,
                u=u,
                terms=term.list,
                starts=start.list,
                dft=S,
                model=models[[best.model.index]]))
}


#' A formula that describes a Finite Fourier Fit
#'
#' See \code{\link{build.term}} for the details of how each term is built.
#'
#' @param response A string or expression that will be the lhs of the formula.
#' @param term.list The terms for the rhs of the formula.
#' @param data The data frame to predict on
#' @param starts The starting values for the \code{\link{nls}} fit.
#' @return an \code{nls} object that represents a Finite Fourier Basis.
#' @importFrom stats as.formula
.build.model <- function(response, term.list, data, starts) {
    return(nls(as.formula(paste(response,
                                "~",
                                paste(term.list, collapse=" + "))),
               data=data,
               start=starts))
}


#' @importFrom stats BIC
.best.fit <- function(fit.list) {
    v <- sapply(fit.list, BIC)
    i <- .argmin(v)
    return(c(index=i, BIC=v[i]))
}
