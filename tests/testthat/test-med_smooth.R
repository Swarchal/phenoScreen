context("med_smooth")

# example data

vals <- rnorm(96)
wells <- num_to_well(1:96)
platemap <- plate_map(data = vals, well = wells)
out <- med_smooth(platemap, plate = 96)


test_that("med_smooth errors when expected",{
    expect_error(med_smooth(iris, plate = 96))
    expect_error(med_smooth(platemap, plate = 1))
})

test_that("med_smooth returns a dataframe",{
    expect_true(is.data.frame(out))
})

test_that("return dataframe is correct size",{
    expect_true(nrow(out) == 96L)
    expect_true(ncol(out) == 2L)
})

# TODO check correct answers (how?)
