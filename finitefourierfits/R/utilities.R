## Copyright (C) 2020 by Landmark Acoustics LLC

#' The index of the minimum element in a container
#'
#' `argmin` acts just like the mathematical concept it is named after.
#'
#' @param x Some collection, probably a [vector()] of some kind.
#' @return the index of the smallest item in the collection.
.argmin <- function(x) {
    return(match(min(x), x))
}


.find.local.peaks <- function(x, threshold.quantile=0.5) {
    lhd <- diff(c(-Inf, x))
    rhd <- diff(c(x, -Inf))
    f <- lhd > 0 & rhd < 0
    return(seq_along(x)[f])
}


#' Angular frequencies for a Discrete Fourier Transform
#'
#' If there are `fft.size` samples in the DFT then the frequencies run
#' from 0 (inclusive) to \eqn{2pi} (exclusive).
#'
#' @param fft.size The size of the DFT
#' @return a vector of floats of length `fft.size` in \eqn{[0, 2\pi)}.
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


.na.on.error <- function(e) {
    return(NA)
}


.to.data <- function(independent.variable,
                     dependent.variable) {
    return(data.frame(x=independent.variable,
                      y=dependent.variable))
}


.find.independent.variable <- function(data) {
    if (is.list(data)) {
        return(data$x)
    }
    return(data)
}
