## Copyright (C) 2020 by Landmark Acoustics LLC

context("Utilities")
library(finitefourierfits)


mono.up <- 1:5
mono.down <- seq(0, -1, by=-0.5)


test_that(".argmin works", {
    expect_equal(.argmin(mono.up), 1)
    expect_equal(.argmin(mono.down), 3)
})


test_that("peak finding works", {
    expect_equal(.find.local.peaks(mono.up), length(mono.up))
    expect_equal(.find.local.peaks(mono.down), 1)
    expect_equal(.find.local.peaks(c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 9)),
                 c(1, 3, 6, 8, 12))
})


test_that("omegas works", {
    expect_equal(omegas(4), c(0, pi/2, pi, 3*pi/2))
    expect_equal(omegas(1), 0)
})


test_that("`.null.on.error` `is.null`", {
    expect_null(.null.on.error())
})


test_that("`.na.on.error` `is.na`", {
    expect_true(is.na(.na.on.error()))
})


x <- seq(0, 0.99, length.out=100)
u <- .domain.shift(400, x)


test_that("domain shift has the right slope", {
    expect_equal(u$slope, 2*pi*100/400)
})


test_that("domain shift has the right intercept", {
    expect_equal(u$intercept, 0.0)
})


test_that("domain shift maps correctly", {
    expect_equal(u$FUN(0), 0)
    expect_equal(u$FUN(0.5), pi/4)
    expect_equal(u$FUN(1), pi/2)
})


u <- rnorm(22)
v <- 0.5*u+rnorm(length(u), 0, 0.5)
X <- .to.data(u, v)


test_that("creating the dummy data frame works", {
    expect_equal(names(X), c("x", "y"))
    expect_equal(nrow(X), length(u))
    expect_equal(u, X$x)
    expect_equal(v, X$y)
})


test_that("the independent variable can be found", {
    expect_equal(u, .find.independent.variable(X))
    expect_equal(v, .find.independent.variable(X$y))
})
