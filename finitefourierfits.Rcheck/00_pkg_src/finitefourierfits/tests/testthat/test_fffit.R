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
    expect_equal(fit$best.index, 1)
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
    expect_equal(fit$best.index, 1)
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
    expect_equal(fit$best.index, 1)
    expect_equal(formula(fit),
                 y ~ a1 * cos(2 * pi * f1 * x - p1))
    expect_equal(coefs.w.unwrapped.phase(fit),
                 c(a1=alpha, f1=phi, p1=.unwrap(psi)),
                 tolerance=0.001)
})


test_that("An `fffit` matches a simple line", {
    m <- 20
    b <- -sqrt(2)
    y <- m*N*x + b + rnorm(N)

    fit <- fffit(N*x, y)

    expect_equal(fit$best.index, 2)
    expect_equal(coefs.w.unwrapped.phase(fit),
                 c(b=b, m=m),
                 tolerance=0.01)
})


test_that("An `fffit` matches a simple parabola", {
    a <- exp(1)
    b <- -sqrt(2)
    y <- (N*x - a) * (N*x - b) + rnorm(N)

    fit <- fffit(N*x, y)

    expect_equal(fit$best.index, 1)
    expect_equal(length(coef(fit)), 3 * fit$best.index)
})


test_that("Two cosine curves are fit by a 6-term `fffit`", {
    a <- 2*c(4, 3)
    f <- c(157, 921)
    p <- c(1, 2)
    y <- a[1]*cos(.tau*x*f[1] - p[1]) + a[2]*cos(.tau*x*f[2] - p[2]) + rnorm(N)

    fit <- fffit(x, y)

    expect_equal(fit$best.index, 2)
    expect_equal(coefs.w.unwrapped.phase(fit),
                 c(a1=a[1], a2=a[2],
                   f1=f[1], f2=f[2],
                   p1=.unwrap(p[1]), p2=.unwrap(p[2])),
                 tolerance=0.001)
})


test_that("`fffit` can handle an irrational transcendantal function", {
    a <- 20
    u <- seq(0, 10, 0.01)
    v <- a * u * exp(-u) + rnorm(length(u))
    fit <- fffit(u, v - mean(v), pad.multiplier=6)

    expect_equal(coefs.w.unwrapped.phase(fit),
                 c(a1=3.49, a2=-0.178, a3=-2.11,
                   f1=0.103, f2=0.832, f3=0.16,
                   p1=.unwrap(1.46), p2=.unwrap(0.89), p3=.unwrap(-2.46)),
                 tolerance=0.001)
})


a <- c(5, 3)
f <- c(7, 2) * Hz / N
y <- a[1] * sin(2*pi*f[1]*x) + a[2]*sin(2*pi*f[2]*x)
fit <- fffit(x, y + rnorm(N))


test_that("prediction works", {
    expect_equal(as.numeric(predict(fit)), y, tolerance=0.1)
})


test_that("extracting coefficients works", {
    expect_equivalent(abs(coef(fit)[1:2]), a, tolerance=0.02)
    expect_equivalent(abs(coef(fit)[2+1:2]), f, tolerance=0.02)
    expect_equivalent(abs(coef(fit)[4+1:2]), rep(pi/2, 2), tolerance=0.02)
})


test_that("ANOVA works", {
    ftable <- anova(fit)
    foo <- strsplit(attr(ftable, "heading")[[2]], "\n")[[1]]
    lapply(seq_along(foo), function(n) {
        s <- foo[[n]]
        pivot <- grepRaw(":", s)
        foo <- substr(s, 1, pivot - 1)
        bar <- substr(s, pivot + 1, nchar(s))
        expect_equal(foo, paste("Model", n))
        expect_equal(formula(bar), formula(fit$models[[n]]))
    })
})


test_that("extracting formulae works", {
    sapply(seq_along(fit$models), function(n) {
        expect_equal(formula(fit, index=n), formula(fit$models[[n]]))
    })
})
