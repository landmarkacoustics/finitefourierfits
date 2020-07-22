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
    for (i in seq_along(a)) {
        expect_equal(a[i], ifelse(i==3, 1, 0))
    }
})


test_that("phases extract correctly", {
    for (i in seq_along(p)) {
        expect_equal(p[i], ifelse(i==3, -pi/2, 0))
    }
})


test_that("the whole fourier.transform thing works", {
    ft <- fourier.transform(X, 1, 1L)
    expect_true("fourier.transform" %in% class(ft))
    expect_equal(ft$dft, Z)
    expect_equal(ft$a, a)
    expect_equal(ft$f, seq(0, 3/8, 1/8))
    expect_equal(ft$p, p)
    expect_equal(ft$magnitude.order, c(3, 1, 2, 4))
})


test_that("multiplier changes the fft.size of a fourier.transform", {
    K <- nextn(length(X), 2)
    for(m in seq_len(8)) {
        expect_equal(fourier.transform(X, 1, m)$fft.size, m*K)
    }
})
