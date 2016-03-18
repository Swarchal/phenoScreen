context("ssmd")

set.seed(123123)

test_that("ssmd returns errors when expected",{
    expect_error(ssmd(1,2,3,4,6))
    expect_error(ssmd(c(1,2,3,4,5)))
    expect_error(ssmd(rnorm(100), c('a', 'b', 'c')))
    expect_error(ssmd(c(1,2), c(1,2)))
})

test_that("warns of different vector lengths",{
    expect_warning(ssmd(rnorm(100), rnorm(1000)))
})

test_that("verbose returns message",{
    expect_message(ssmd(rnorm(100), rnorm(100), verbose = TRUE))
})

x <- rnorm(100)
out <- ssmd(x, x)

test_that("returns expected answers",{
    expect_true(abs(ssmd(rnorm(1000, 0.1), rnorm(1000, 100))) > 5)
    expect_true(out == 0)
})
