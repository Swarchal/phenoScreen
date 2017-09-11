context("scale_features")

# create example data
N_PLATES = 5
wells = rep(platetools::num_to_well(1:96), N_PLATES)
plate_id = rep(c("plate_1", "plate_2", "plate_3", "plate_4", "plate_5"),
        each = 96)
val1 = rnorm(96 * N_PLATES, 10, 10)
val2 = rnorm(96 * N_PLATES, 1, 100)
comps = c(rep("cmpd", 80), rep("DMSO", 16))
compound = rep(comps, N_PLATES)


df = data.frame(Metadata_well = wells,
         Metadata_plate_id = plate_id,
         Metadata_compound = compound,
         val1, val2)


output <- df %>% scale_features()

test_that("returns dataframe of the same size", {
    expect_equal(dim(df), dim(output))
    expect_equal(colnames(df), colnames(output))
})

test_that("features are scaled", {
    just_features = output[, c("val1", "val2")]
    col_means = as.vector(apply(just_features, 2, mean))
    col_sd = as.vector(apply(just_features, 2, sd))

    expect_equal(col_means, c(0, 0), tolerance=1e-3)
    expect_equal(col_sd, c(1, 1), tolerance=1e-3)
})