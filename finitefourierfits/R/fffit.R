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
#' my.fit <- fffit(x, y)
#' @export
#' @importFrom stats BIC fft nextn nls
fffit <- function(x, y, model.selector=BIC, max.terms=10) {

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
                                     .to.data(u, x, y),
                                     start.list[1:i]),
                        error=.null.on.error)
        if (!is.null(tmp)) {
            models <- append(models, list(tmp))
        }
    }
    scores <- sapply(models, model.selector)
    best.model.index <- .argmin(scores)
    result <- list(response=lhs,
                   u=u,
                   terms=term.list,
                   starts=start.list,
                   dft=S,
                   models=models,
                   selection.scores=scores,
                   best.index=best.model.index)

    class(result) <- append(class(result), "fffit")

    invisible(result)
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


.to.data <- function(transformation,
                     independent.variable,
                     dependent.variable) {
    return(data.frame(w=transformation(independent.variable),
                      y=dependent.variable))
}


.find.independent.variable <- function(data) {
    if (is.list(data)) {
        return(data$x)
    }
    return(data)    
}


#' @importFrom stats BIC
.best.fit <- function(fit.list) {
    v <- sapply(fit.list, BIC)
    i <- .argmin(v)
    return(c(index=i, BIC=v[i]))
}


#' Predicting from Finite Fourier Fits
#'
#' `predict.fffit` produces predicted values, obtained by evaluating the fit
#' in the context of the `newdata`. This will give nonsensical results if the
#' input domain is not within the bounds of the original analysis's domain.
#'
#' @param object An `fffit` object
#' @param newdata A data frame with new independent and dependent variables.
#' @param index optional, a specific model to summarize
#' @param ... Additional arguments to `predict.nls`
#' @return A numeric vector of predictions
#' @seealso `\link{predict.nls}`
#' @importFrom stats predict fitted
#' @export
predict.fffit <- function(object, newdata=NULL, index=NULL, ...) {
    if (is.null(index)) {
        index <- object$best.index
    }
    mo <- object$models[[index]]
    if (is.null(newdata)) {
        return(fitted(mo))
    }
    x <- .find.independent.variable(newdata)
    return(predict(mo, data.frame(w=object$u(x))))
}


#' Comparing the Quality of the Possible Fits
#'
#' `anova.fffit` prints an ANOVA table of all of the models that the fit made.
#'
#' @param object An fffit object
#' @param ... optional, additional arguments to anova. DON'T USE
#' @return an anova object
#' @seealso `\link{anova}`, `\link{nls}`
#' @importFrom stats anova
#' @export
anova.fffit <- function(object, ...) {
    if (length(list(...))) {
        stop("don't pass any ellipsis arguments to anova.fffit")
    }
    do.call(anova, c(object$models))
}


#' Demonstrating the Quality of the Best Fit
#'
#' `summary.fffit` prints an summary table of the model that fit best.
#'
#' @param object An fffit object
#' @param index optional, a specific model to summarize
#' @param ... optional, additional arguments to `\link{summary}`
#' @return a summary object
#' @seealso `\link{summary}`, `\link{nls}`
#' @export
summary.fffit <- function(object, index=NULL, ...) {
    if (is.null(index)) {
        index <- object$best.index
    }
    summary(object$models[[index]], ...)
}


#' The Coefficients of the Best Fit
#'
#' `coef.fffit` returns the coefficients of the best-fit model
#'
#' @param object An fffit object
#' @param index optional, a specific model to extract coefficients from.
#' @param ... optional, additional arguments to `\link{coef}`
#' @return a vector of coefficients
#' @seealso `\link{coef}`, `\link{nls}`
#' @importFrom stats coef
#' @export
coef.fffit <- function(object, index=NULL, ...) {
    if (is.null(index)) {
        index <- object$best.index
    }
    coef(object$models[[index]], ...)
}


#' The Formula of the Best Fit
#'
#' `formula.fffit` returns the formula of the best-fit model
#'
#' @param x An fffit object
#' @param index optional, a specific model to get the formula for
#' @param ... optional, additional arguments to `\link{formula}`
#' @return a formula
#' @seealso `\link{formula}`, `\link{nls}`
#' @importFrom stats formula
#' @export
formula.fffit <- function(x, index=NULL, ...) {
    if (is.null(index)) {
        index <- x$best.index
    }
    formula(x$models[[index]], ...)
}
