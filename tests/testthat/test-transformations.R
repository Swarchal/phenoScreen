context("transformations")

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
