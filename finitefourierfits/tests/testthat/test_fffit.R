## Copyright (C) 2020 by Landmark Acoustics LLC

context("assembling FF Fits")
library(finitefourierfits)
set.seed(42)


test_that("`fffit` stops when length(x) != length(y)", {
    expect_error(fffit(1:4, rnorm(3)))
})


Hz <- 44100
N <- 512
x <- seq(-N / (2*Hz), (N-1) / (2*Hz), 1 / Hz)


coefs.w.unwrapped.phase <- function(fit) {
    fit.coefficents <- coef(fit)
    nombres <- names(fit.coefficents)
    fit.coefficents <- ifelse(grepl("p", nombres),
                              .unwrap(fit.coefficents),
                              fit.coefficents)
    names(fit.coefficents) <- nombres
    return(fit.coefficents)
}


alpha <- 100
phi <- 5 * Hz / N
psi <- pi/7

test_that("An `fffit` matches a simple cosine curve", {

    y <- alpha*cos(2*pi*x*phi - psi) + rnorm(N)

    fit <- fffit(x, y)

    expect_true("fffit" %in% class(fit))
    expect_equal(names(fit),
                 c("dft", "terms", "models", "selection.scores", "best.index"))
    expect_equal(fit$best.index, 3)
    expect_equal(formula(fit),
                 y ~ a1 * cos(2 * pi * f1 * x - p1))
    expect_equal(coefs.w.unwrapped.phase(fit),
                 c(a1=alpha, f1=phi, p1=.unwrap(psi)),
                 tolerance=0.001)
})


test_that("An `fffit` of a simple cosine curve with a negative phase", {

    y <- alpha*cos(2*pi*x*phi + psi) + rnorm(N)

    fit <- fffit(x, y)

    expect_true("fffit" %in% class(fit))
    expect_equal(names(fit),
                 c("dft", "terms", "models", "selection.scores", "best.index"))
    expect_equal(fit$best.index, 3)
    expect_equal(formula(fit),
                 y ~ a1 * cos(2 * pi * f1 * x - p1))
    expect_equal(coefs.w.unwrapped.phase(fit),
                 c(a1=alpha, f1=phi, p1=.unwrap(-psi)),
                 tolerance=0.001)
})


test_that("An `fffit` of a simple cosine curve with a nonzero mean", {

    mu <- 42

    y <- alpha*cos(2*pi*x*phi - psi) + rnorm(N) + mu

    fit <- fffit(x, y-mu)

    expect_true("fffit" %in% class(fit))
    expect_equal(names(fit),
                 c("dft", "terms", "models", "selection.scores", "best.index"))
    expect_equal(fit$best.index, 3)
    expect_equal(formula(fit),
                 y ~ a1 * cos(2 * pi * f1 * x - p1))
    expect_equal(coefs.w.unwrapped.phase(fit),
                 c(a1=alpha, f1=phi, p1=.unwrap(psi)),
                 tolerance=0.001)
})


test_that("An `fffit` matches a simple line", {
    m <- 2048
    b <- -sqrt(2)
    y <- m*N*x + b + rnorm(N)

    fit <- fffit(N*x, y)

    expect_equal(fit$best.index, 2)
    expect_equal(coef(fit), c(`(Intercept)`=b, x=m), tolerance=0.001)
})


test_that("An `fffit` matches a simple parabola", {
    a <- exp(1)
    b <- -sqrt(2)
    y <- (N*x - a) * (N*x - b) + rnorm(N)

    fit <- fffit(N*x, y)

    expect_equal(fit$best.index, 3)
    expect_equal(length(coef(fit)), 3 * (fit$best.index-2))
})


test_that("Two cosine curves are fit by a 6-term `fffit`", {
    a <- 2*c(4, 3)
    f <- c(157, 921)
    p <- c(-1, 2)
    y <- a[1]*cos(.tau*x*f[1] + p[1]) + a[2]*cos(.tau*x*f[2] + p[2]) + rnorm(N)

    fit <- fffit(x, y)

    expect_equal(fit$best.index, 4)
    expect_equal(coefs.w.unwrapped.phase(fit),
                 c(a1=a[1], a2=a[2],
                   f1=f[1], f2=f[2],
                   p1=.unwrap(p[1]), p2=.unwrap(p[2])),
                 tolerance=0.001)
})
