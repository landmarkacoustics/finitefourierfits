## Copyright (C) 2020 by Landmark Acoustics LLC

#' Half of the input, rounded down to the nearest integer.
#'
#' \code{half.fft} returns the pivot index between the positive and negative
#' frequencies in the output of \code{\link{fft}}.
#'
#' @param fft.size The length of the Discrete Fourier Transform window.
#' @return An integer equal to half of \code{fft.size}, rounded down.
half.fft <- function(fft.size) {
    return(as.integer(fft.size/2))
}


#' @export
.amplitudes <- function(z) {
    fft.size <- length(z)
    ix <- half.fft(fft.size)
    return(c(Re(z[1]) + Re(z[ix+1]), 2*Mod(z[2:ix])) / fft.size)
}


#' @export
.phases <- function(z) {
    return(Arg(z[1:half.fft(length(z))]))
}


#' The Target Fft Size for an `\link{fffit}` Object Based on Input Size
#'
#' Rounding up to the nearest power of two is traditional for computing the
#' length of a DFT
#'
#' @param x An array, vector, or integer, which will be used to calculate the
#' input length using `dim`, `length`, or the integer itself, respectively.
#' @param multiplier optional, an integer multiplier >= 1. defaults to 4
#' @return The length of the input, rounded up to the nearest power of two and
#' multiplied by `multiplier`.
#' @seealso `\link{nextn}`
#' @export
padded.fft.size <- function(x, multiplier=4L) {
    if (!is.integer(multiplier) || multiplier < 1){
        stop("Invalid value of `multiplier`: it must be an integer >= 1")
    }

    N <- dim(x)
    if (!is.null(N)) {
        N <- N[1]
    }
    else {
        N <- length(x)
        if (N == 1) {
            N <- x
        }
    }

    return(multiplier*nextn(N, 2))
}
