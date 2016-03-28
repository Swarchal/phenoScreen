context("plate_matix")

# example data
x <- 1:384
wells <- num_to_well(1:384, plate = 384)
out <- plate_matrix(data = x, well = wells, plate = 384)

test_that("returns error when expected",{
    expect_error(plate_matrix(data = x,
			      well = wells,
			      plate = 1)
    )
})

test_that("returns a matrix",{
    expect_is(out, 'matrix')
})

test_that("returns correct size matrix",{
    expect_equal(prod(dim(out)), 384L)
    expect_equal(ncol(out), 24L)
    expect_equal(nrow(out), 16L)
})

test_that("returns expected values",{
    expect_equal(matrix(1:384, ncol = 24, nrow = 16, byrow = TRUE), out)
})
