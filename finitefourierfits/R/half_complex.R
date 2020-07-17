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


amplitudes <- function(z) {
    fft.size <- length(z)
    ix <- half.fft(fft.size)
    return(c(Re(z[1]) + Re(z[ix+1]), 2*Mod(z[2:ix])) / fft.size)
}


phases <- function(z) {
    return(Arg(z[1:half.fft(length(z))]))
}
