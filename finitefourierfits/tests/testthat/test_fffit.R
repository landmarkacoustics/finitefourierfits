## Copyright (C) 2020 by Landmark Acoustics LLC

context("assembling FF Fits")
library(finitefourierfits)


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
