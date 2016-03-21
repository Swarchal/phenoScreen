context("z_factor")

a <- rnorm(10000, 1)
b <- rnorm(10000, 100)
out <- z_factor(a, b)

test_that("z_factor returns a number",{
    expect_true(is.numeric(out))
    expect_equal(length(out), 1L)
    expect_true(is.numeric(z_factor(rnorm(100), rnorm(10000))))
})

test_that("z_factor returns expected value",{
    expect_true(z_factor(rnorm(1000, 1), rnorm(1000, 1)) < 0)
    expect_true(out > 0.5)
})
