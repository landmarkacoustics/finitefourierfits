## Copyright (C) 2020 by Landmark Acoustics LLC

context("Utilities")
library(finitefourierfits)

mono.up <- 1:5
mono.down <- seq(0, -1, by=-0.5)

test_that("argmin works", {
    expect_equal(argmin(mono.up), 1)
    expect_equal(argmin(mono.down), 3)
})


test_that("omegas works", {
    expect_equal(omegas(4), c(0, pi/2, pi, 3*pi/2))
    expect_equal(omegas(1), 0)
})
