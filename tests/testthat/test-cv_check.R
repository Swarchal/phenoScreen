context("cv_check")

# example data
set.seed(123)

a <- rnorm(100, 10, 1)
b <- rnorm(100, 10, 10)
vals <- c(a, b)
grouping <- c(rep('a', 100), rep('b', 100))
df <- data.frame(vals, grouping)

out <- cv_check(data = df$vals,
		group = df$grouping)

test_that("returns dataframe",{
    expect_is(out, 'data.frame')
})

test_that("returns expected columns",{
    expect_equal(ncol(out), 2L)
    expect_equal(nrow(out), 2)
})

test_that("returns expected values",{
    expect_is(out$CV, 'numeric')
    expect_true(out[1,2] < out[2,2])
})
