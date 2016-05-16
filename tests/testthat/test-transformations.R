context("transformations")
set.seed(12321)

test_that("returns numbers", {
    x <- rnorm(1000)
    out <- glog(x)
    expect_equal(length(x), length(out))
    expect_true(is.numeric(out))
})

test_that("returns a matrix", {
    mat <- matrix(rnorm(10000), ncol = 100)
    mat_out <- glog(mat)
    expect_true(is.matrix(mat_out))
    expect_equal(ncol(mat_out), 100)
})

test_that("roughly normalises data", {
    # create a skewed distribution
    a <- rnorm(1000)^2
    # w value from shapiro wilk test, high = normal distribution
    w_a <- shapiro.test(a)$statistic
    # glog transformation and w value
    glog_a <- glog(a)
    w_glog_a <- shapiro.test(glog_a)$statistic
    # check transformation if more normal than original data
    expect_true(w_a < w_glog_a)
})
