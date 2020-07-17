## Copyright (C) 2020 by Landmark Acoustics LLC

#' One term from a Fourier basis
#'
#' A finite Fourier fit uses some of the terms from a Discrete Fourier
#' Transform to approximate a smooth fit to some data. This function
#' renders one such part of a term.
#'
#' @param index The integer that corresponds to an item in the DFT.
#' @param phase The actual phase of the item from the DFT.
#' @param variable.name An optional symbol that defaults to 'a'.
#' @return A string representation of the term
#' @examples
#' build.term(1, 0)
#' build.term(42, 2.71828)
#' @seealso
#' \code{\link{fft}} for calculating a DFT
#' \code{\link{Arg}} for calculating the phase of an item in DFT
#' \code{\link{as.formula}} for using the output to build a formula
#' @export
build.term <- function(index, phase, variable.name="a") {
    if (index>1) {
        variable <- paste(variable.name, index, sep="")
        result <- paste(variable,
                        " * cos(",
                        index - 1, " * w ",
                        ifelse(phase < 0, "-", "+"), " ",
                        abs(phase),
                        ")", sep="")
    }
    else{
        variable <- result <- "b"
    }
    names(result) <- variable
    return(result)
}

#' A list of Fourier basis terms in the requested order
#'
#' See \code{\link{build.term}} for the details of how each term is built.
#'
#' @param indices.to.use Which terms of the DFT to use in the formula.
#' @param all.phases Every phase from the DFT.
#' Elements are chosen with \code{indices.to.use}.
#' @return a list of terms representing part of a Finite Fourier Basis.
#' @examples
#' build.term.list(1:4, rnorm(10) %% pi)
#' @export
build.term.list <- function(indices.to.use, all.phases) {
    terms <- list()
    for (i in indices.to.use) {
        terms <- append(terms, build.term(i, all.phases[i]))
    }
    return(terms)
}
