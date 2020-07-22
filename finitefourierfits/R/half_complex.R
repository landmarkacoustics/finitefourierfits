## Copyright (C) 2020 by Landmark Acoustics LLC

#' Half of the input, rounded down to the nearest integer.
#'
#' \code{half.fft} returns the pivot index between the positive and negative
#' frequencies in the output of [fft()].
#'
#' @param fft.size The length of the Discrete Fourier Transform window.
#' @return An integer equal to half of 'fft.size`, rounded down.
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


#' Compute a DFT, and Some Helpful Summaries and Descriptors
#'
#' `fourier.transform` zero-pads `x`, calculates the normal real to half-complex
#' DFT of the padded vector, and then stores the amplitudes and phases, and
#' amplitude rank of the non-negative frequency terms.
#'
#' @param x A numeric vector to be Fourier transformed
#' @param sample.rate The sample rate of the time series underlying `x`.
#' @param multiplier optional, controls how much padding `x` gets.
#' @return an S3 object of class `fourier.transform` with the following names:
#'
#' dft
#' ~ The complex-valued [dft()] of x after it has been right-padded with zeros.
#'
#' fft.size
#' ~ The length of `dft`
#'
#' sample.rate
#' ~ This isn't actually used in the ctor, but it`s super useful to have here.
#'
#' a
#' ~ The real-valued magnitudes of the non-negative frequency term.
#'
#' f
#' ~ Frequencies in \[0, Nyquist\)
#' 
#' p
#' ~ The real-valued phases of each non-negative frequency term.
#'
#' magnitude.order
#' ~ Indices into `a` that sort it from largest to smallest.
#'
#' @seealso [dft()], [.amplitudes()], [.phases()], [half.fft()]
#' @export
fourier.transform <- function(x, sample.rate, multiplier=4L) {
    N <- length(x)
    fft.size <- multiplier * nextn(N, 2)
    result <- list(dft=fft(c(x, rep(0, fft.size - N))),
                   fft.size=fft.size,
                   sample.rate=sample.rate)
    result$a <- .amplitudes(result$dft)
    result$f <- 0:(half.fft(fft.size) - 1)*(sample.rate/fft.size)
    result$p <- .phases(result$dft)
    result$magnitude.order <- order(result$a, decreasing=TRUE)
    class(result) <- append(class(result), "fourier.transform")
    return(result)
}
