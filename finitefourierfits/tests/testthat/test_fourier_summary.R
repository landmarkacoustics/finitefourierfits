## Copyright (C) 2020 by Landmark Acoustics LLC

context("Frequency Domain Functions")
library(finitefourierfits)


test_that("the `half.fft` syntactic sugar is correct", {
    expect_equal(half.fft(42), 21L)
    expect_equal(half.fft(2*pi), 3L)
})


X <- c(0, 1, 0, -1, 0, 1, 0, -1)
Z <- fft(X)
a <- .amplitudes(Z)
p <- .phases(Z)


test_that("amplitudes extract correctly", {
    sapply(seq_along(a), function(i) {
        expect_equal(a[i], ifelse(i==3, 1, 0))
    })
})


test_that("phases extract correctly", {
    sapply(seq_along(p), function(i) {
        expect_equal(p[i], ifelse(i==3, Arg(Z[i]), 0))
    })
})


test_that("sample rate calculates correctly", {
    expect_equal(.sample.rate(seq(0, 2*pi, pi/100)), 100/pi)
    expect_equal(.sample.rate(1:20), 1)
    expect_equal(.sample.rate(seq(-2, 2, length.out=401)), 100)
})


test_that("the whole fourier.summary thing works", {
    ft <- fourier.summary(X, 1, 1L)
    expect_true("fourier.summary" %in% class(ft))
    expect_equal(ft$dft, Z)
    expect_equal(ft$a, a)
    expect_equal(ft$f, seq(0, 3/8, 1/8))
    expect_equal(ft$p, p)
    expect_equal(ft$magnitude.order, 3)
})


test_that("multiplier changes the fft.size of a fourier.summary", {
    K <- nextn(length(X), 2)
    sapply(seq_len(8), function(m) {
        expect_equal(fourier.summary(X, 1, m)$fft.size, m*K)
    })
})
