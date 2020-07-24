## Copyright (C) 2020 by Landmark Acoustics LLC

context("building formulas for FF Fits")
library(finitefourierfits)

test_that("the string version of a term is a fully parameterized cosine", {
    expect_equal(.trig.term(3),
                 "a3*cos(2*pi*f3*x - p3)")
    expect_equal(.trig.term(42),
                 "a42*cos(2*pi*f42*x - p42)")
})


test_that("named items work as expected", {
    a <- c(a1=3, a2=1, a3=4, a4=1, a5=5)
    for (i in seq_along(a)) {
        expect_equal(.named.item(as.numeric(a[i]), "a", i),
                     a[i])
    }
})


Hz <- 44100
fft.size <- 512
x <- seq(0, fft.size-1)/Hz
df <- Hz / fft.size
phi <- 5 * df

DFT <- fourier.summary(cos(2*pi * phi * x), Hz, 1)

test_that("the DFT fixture is set up as expected", {
    expect_equal(DFT$fft.size, fft.size)
    expect_equal(DFT$magnitude.order[1], 6)
})


test_that("frequency estimates are correct", {
    efs <- .frequency.guesses(1:4, DFT)

    for (i in seq_along(efs)) {
        expect_equal(as.numeric(efs[i]),
                     df * (DFT$magnitude.order[i]-1))
        expect_equal(names(efs)[i], paste("f", i, sep=""))
    }
})


test_that("the biggest term builds correctly", {
    term <- fffterm(1, DFT)
    expect_true("fffterm" %in% class(term))
    lapply(list("a", "f", "p"), function(n) {
        expect_equal(term[[n]], .named.item(DFT[[n]][6], n, 1))
    })
    expect_equal(term$term, "a1*cos(2*pi*f1*x - p1)")
})


test_that("the zero-frequency term builds correctly", {
    one <- match(1, DFT$magnitude.order)
    term <- fffterm(one, DFT)
    lapply(list("f", "p"), function(n) {
        expect_true(is.na(term[[n]]))
    })
    expect_equal(term$a, .named.item(DFT$a[one], "b", ""))
})


test_that("the second biggest term builds correctly", {
    term <- fffterm(2, DFT)
    expect_true("fffterm" %in% class(term))
    lapply(list("a", "f", "p"), function(n) {
        expect_equal(term[[n]], .named.item(DFT[[n]][233], n, 2))
    })
    expect_equal(term$term, "a2*cos(2*pi*f2*x - p2)")
})


test_that("`.get.part` works", {
    term <- fffterm(4, DFT)
    lapply(names(term), function(n) {
        expect_equal(.get.part(term, n), term[[n]])
    })
})


term.ranks <- c(1:3, match(1, DFT$magnitude.order))
term.list <- lapply(term.ranks, fffterm, DFT)


test_that("the formula builds correctly", {
    expect_equal(.build.formula("y", term.list),
                 formula(y ~ a1*cos(2*pi*f1*x - p1) +
                             a2*cos(2*pi*f2*x - p2) +
                             a3*cos(2*pi*f3*x - p3) +
                             b))
})


a.starts <- .concatenate.starts("a", term.list)
test_that("a concatenates correctly", {
    expect_equal(class(a.starts), "list")
    expect_equal(names(a.starts), c("a1", "a2", "a3", "b"))
    for (i in seq_along(a.starts)) {
        expect_equal(a.starts[[i]],
                     with(DFT, a[magnitude.order[term.ranks[i]]]))
    }
})


f.starts <- .concatenate.starts("f", term.list)
test_that("f concatenates correctly", {
    expect_equal(class(f.starts), "list")
    expect_equal(names(f.starts), c("f1", "f2", "f3"))
    for (i in seq_along(f.starts)) {
        expect_equal(f.starts[[i]],
                     with(DFT, f[magnitude.order[term.ranks[i]]]))
    }
})


p.starts <- .concatenate.starts("p", term.list)
test_that("p concatenates correctly", {
    expect_equal(class(p.starts), "list")
    expect_equal(names(p.starts), c("p1", "p2", "p3"))
    for (i in seq_along(p.starts)) {
        expect_equal(p.starts[[i]],
                     with(DFT, p[magnitude.order[term.ranks[i]]]))
    }
})


test_that("A start list for `nls` builds correctly from a term list", {
    expect_equal(.build.starts(term.list),
                 c(a.starts, f.starts, p.starts))
})
