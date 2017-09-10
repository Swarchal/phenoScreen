context("normalise")

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


df_out_div = df %>%
    group_by(Metadata_plate_id) %>%
    normalise(Metadata_compound, method="divide")


test_that("returns a dataframe",{
    expect_is(df_out_div, "data.frame")
    expect_equal(nrow(df_out_div), N_PLATES * 96)
})

df_out_sub = df %>% normalise(Metadata_compound, method="subtract")

test_that("subtract returns a dataframe",{
    expect_is(df_out_sub, "data.frame")
})


test_that("normalize alias works", {
    normalise_output = normalise(df, Metadata_compound)
    normalize_output = normalize(df, Metadata_compound)
    expect_equal(normalise_output, normalize_output)
})


test_that("divide and subtract return different answers",{
    expect_true(any(df_out_sub != df_out_div))
})


test_that("returns error for incorrect method",{
    expect_error(
        df %>% normalise(Metadata_compound,
                         method="invalid_method")
         )
})



# TODO: actually test the data is normalised