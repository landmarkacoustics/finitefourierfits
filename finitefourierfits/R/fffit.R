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
#' @param pad.multiplier Integer, optional, how much zero-padding to add
#' @return an FFFit model
#' @examples
#' x <- rnorm(10)
#' y <- 10*x + rnorm(10)
#' my.fit <- fffit(x, y)
#' @export
#' @importFrom stats BIC fft nextn nls
fffit <- function(x, y,
                  model.selector=BIC,
                  max.terms=10,
                  pad.multiplier=2) {

    N <- length(x)
    if (length(y) != N) {
        stop("`x` and `y` must be the same length.")
    }

    transform <- fourier.summary(y, .sample.rate(x), pad.multiplier)

    term.list <- lapply(seq_len(max.terms), fffterm, transform)

    datums <- .to.data(x, y)

    models <- lapply(seq_along(term.list),
                     function(i) {
                         these.terms <- term.list[seq_len(i)]
                         tryCatch(nls(.build.formula("y", these.terms),
                                      datums,
                                      .build.starts(these.terms)),
                                  error=.na.on.error)
                     })
    models <- models[!is.na(models)]

    fallback.models <- list(lm(y ~ 1, datums),
                            lm(y ~ x, datums))

    models <- c(fallback.models, models)

    scores <- sapply(models, model.selector)

    best.model.index <- .argmin(scores)

    result <- list(dft=transform,
                   terms=term.list,
                   models=models,
                   selection.scores=scores,
                   best.index=best.model.index)

    class(result) <- append(class(result), "fffit")

    invisible(result)
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
#' @seealso [`predict.nls`]
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
    return(predict(mo, data.frame(w=object$u$FUN(x))))
}


#' Comparing the Quality of the Possible Fits
#'
#' `anova.fffit` prints an ANOVA table of all of the models that the fit made.
#'
#' @param object An fffit object
#' @param ... optional, additional arguments to anova. DON'T USE
#' @return an anova object
#' @seealso [anova()], [nls()]
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
#' @param ... optional, additional arguments to [summary()]
#' @return a summary object
#' @seealso [summary()], [nls()]
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
#' @param ... optional, additional arguments to [coef()]
#' @return a vector of coefficients
#' @seealso [coef()], [nls()]
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
#' @param ... optional, additional arguments to [formula()]
#' @return a formula
#' @seealso [formula()], [nls()]
#' @importFrom stats formula
#' @export
formula.fffit <- function(x, index=NULL, ...) {
    if (is.null(index)) {
        index <- x$best.index
    }
    formula(x$models[[index]], ...)
}
