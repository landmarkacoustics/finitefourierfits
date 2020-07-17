## Copyright (C) 2020 by Landmark Acoustics LLC

context("Utilities")
library(finitefourierfits)


mono.up <- 1:5
mono.down <- seq(0, -1, by=-0.5)


test_that(".argmin works", {
    expect_equal(.argmin(mono.up), 1)
    expect_equal(.argmin(mono.down), 3)
})


test_that("omegas works", {
    expect_equal(omegas(4), c(0, pi/2, pi, 3*pi/2))
    expect_equal(omegas(1), 0)
})


test_that("finding the name of `y` works", {
    expect_equal(.somehow.get.the.name.of.y(pi), "y")
    expect_equal(.somehow.get.the.name.of.y("y"), "y")
    expect_equal(.somehow.get.the.name.of.y(NULL), "y")
})


test_that("`.null.on.error` `is.null`", {
    expect_identical(.null.on.error(), NULL)
})


x <- seq(0, 0.99, length.out=100)
u <- .domain.shift(400, x)


test_that("domain shift has the right slope", {
    expect_equal(mean(diff(u(x))), .tau/400)
})


test_that("domain shift maps correctly", {
    expect_equal(u(0), 0)
    expect_equal(u(0.5), pi/4)
    expect_equal(u(1), pi/2)
})
