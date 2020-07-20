## Copyright (C) 2020 by Landmark Acoustics LLC

context("building formulas for FF Fits")
library(finitefourierfits)


test_that("the constant term has no cosine", {
    expect_equal(build.term(1, 42), c(b="b"))
})


test_that("cosine terms have the form a[n]cos(th+ph)", {
    expect_equal(build.term(1, 1),
                     c(b="b"))
    expect_equal(build.term(42, 3.14),
                     c(a42="a42 * cos(41 * w + 3.14)"))
    expect_equal(build.term(3, -99.9),
                     c(a3="a3 * cos(2 * w - 99.9)"))
})


test_that("one can change the variable name with the third argument", {
    expect_equal(build.term(1, 1, "zed"),
                     c(b="b"))
    expect_equal(build.term(2, -2.2, "b"),
                     c(b2="b2 * cos(1 * w - 2.2)"))
    expect_equal(build.term(2, 1, "zed"),
                     c(zed2="zed2 * cos(1 * w + 1)"))
    expect_equal(build.term(2, -3.14, "7"),
                     c(`72`="72 * cos(1 * w - 3.14)"))
})



phi <- c(3.14, -1, 2.71828, -42 %% pi, sqrt(2))

test_that("term lists with constant terms drop the cosine", {
    expect_equal(build.term.list(1, phi),
                 list(b="b"))
})

test_that("complicated terms agglomerate correctly", {
    expect_equal(build.term.list(1:4, phi),
                 list(b="b",
                      a2="a2 * cos(1 * w - 1)",
                      a3="a3 * cos(2 * w + 2.71828)",
                      a4="a4 * cos(3 * w + 1.9822971502571)"))
})

test_that("rounding isn't an issue when building term lists", {
    expect_equal(build.term.list(c(1, 5), phi),
                 list(b="b",
                      a5=paste("a5 * cos(4 * w + ", phi[5], ")",
                               sep="")))
})
