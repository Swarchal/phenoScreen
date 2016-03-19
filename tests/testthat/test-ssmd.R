context("ssmd")

set.seed(123123)

test_that("ssmd returns errors when expected",{
    expect_error(ssmd(1,2,3,4,6))
    expect_error(ssmd(c(1,2,3,4,5)))
    expect_error(ssmd(rnorm(100), c('a', 'b', 'c')))
    expect_error(ssmd(c(2), c(1,2)))
})

test_that("warns of different vector lengths",{
    expect_warning(ssmd(rnorm(100), rnorm(1000)))
})

# test_that("verbose returns message",{
#     expect_message(ssmd(rnorm(100), rnorm(100), verbose = TRUE),
# 	"No effect")
# })


test_that("returns expected values",{
    expect_equal(ssmd(rnorm(1e7), rnorm(1e7)), 0, tolerance = 1e-3)
    expect_true(abs(ssmd(rnorm(100, 0.1), rnorm(100, 100))) > 5)
})
