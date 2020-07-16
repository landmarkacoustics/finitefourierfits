## Copyright (C) 2020 by Landmark Acoustics LLC

#' Fit a Finite Fourier Basis to Data
#'
#' Sometimes there seems to be a clear functional relationship between two
#' variables in a data set, but it is poorly approximated by polynomials. A
#' Fourier basis may be able to help. It is, basically, a linear combination
#' of harmonically-related, phase-shifted, weighted cosine curves. This package
#' uses a very simple-minded approach to finding a basis with few curves.
#'
#' @docType package
#' @name finitefourierfits
NULL


## x <- seq(-2, 2, length.out=401)
## N <- length(x)
## fft.size <- 4 * nextn(N, 2)
## half <- half.fft(fft.size)
## omegas <- frequencies(fft.size)
## b <- x[1]
## Hz <- (N-1)/(x[N]-b)
## d.omega <- (fft.size-1)/omegas[fft.size]
## m <- Hz / d.omega
## u <- function(x){m*(x-b)}
## y <- (x-rnorm(1))*(x-rnorm(1))*(x-rnorm(1))
## mu <- mean(y)
## S <- fft(c(y - mu, rep(0, fft.size-N)))
## a <- amplitudes(S)
## p <- phases(S)
## mag.order <- order(a, decreasing=TRUE)
## step.fits <- list()
## for(i in 1:10){
##     ix <- mag.order[1:i]
##     step.fits[[i]] <- tryCatch(nls(build.formula("y-mu",
##                                                  build.rhs.list(ix,
##                                                                 p)),
##                                    start=build.starts(ix,
##                                                       a)),
##                                error=function(e) NULL)
## }
    
## step.fits[sapply(step.fits, function(x)!is.null(x))]
## tops <- best.fit(step.fits)
## plot(x, y, las=1, col="blue", xlab=expression(x), ylab=expression(f(x)))
## lines(x, predict(step.fits[tops[1]])+mu, col="orange", lwd=2)
