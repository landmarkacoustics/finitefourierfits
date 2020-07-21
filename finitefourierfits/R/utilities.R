## Copyright (C) 2020 by Landmark Acoustics LLC

#' The index of the minimum element in a container
#'
#' \code{argmin} acts just like the mathematical concept it is named after.
#'
#' @param x Some collection, probably a \code{\link{vector}} of some kind.
#' @return the index of the smallest item in the collection.
.argmin <- function(x) {
    return(match(min(x), x))
}

#' Angular frequencies for a Discrete Fourier Transform
#'
#' If there are \code{fft.size} samples in the DFT then the frequencies run
#' from 0 (inclusive) to \eqn{2\pi} (exclusive).
#'
#' @param fft.size The size of the DFT
#' @return a vector of floats of length \code{fft.size} in \eqn{[0, 2\pi)}.
#' @examples
#' omegas(4)
#' @export
omegas <- function(fft.size) {
    return(2 * pi * seq(0, fft.size - 1) / fft.size)
}


.domain.shift <- function(fft.size, domain.variable) {
    Hz <- 1 / mean(diff(domain.variable))
    m <- 2 * pi * Hz / fft.size
    b <- domain.variable[1] * m
    return(list(
        slope=m,
        intercept=b,
        FUN=function(x) {
        m * (x - b)
    }))
}


.null.on.error <- function(e) {
    return(NULL)
}
