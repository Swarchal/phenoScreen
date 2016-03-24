context("scale_features")

# example data
v1 <- rnorm(96, 10, 10)
v2 <- rnorm(96, 100, 100)
meta <- sample(letters, 96, replace = TRUE)
df <- data.frame(Metadata_x = meta,
		 v1, v2)
out <- scale_features(df)

test_that("scale_features returns a dataframe",{
    expect_is(out, 'matrix')
})

test_that("scale features returns correct dimensions",{
    expect_equal(nrow(out), 96L)
    expect_equal(ncol(out), 2L) # only returns scaled columns
})

test_that("scales values",{
   expect_equal(mean(out[,1]), 0L, tolerance = 1e-3)
   expect_equal(mean(out[,2]), 0L, tolerance = 1e-3)
})
