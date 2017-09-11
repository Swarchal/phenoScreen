context("collapse")

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


df_out = df %>%
    group_by(Metadata_plate_id, Metadata_well) %>%
    collapse()

df_out2 = df %>%
    group_by(Metadata_well) %>%
    collapse()


test_that("correct number of rows", {
    expect_is(df_out, "data.frame")
    expect_equal(
        n_groups(group_by(df, Metadata_plate_id, Metadata_well)),
        nrow(df_out)
        )
    expect_equal(
        n_groups(group_by(df, Metadata_well)),
        nrow(df_out2)
    )
    expect_equal(colnames(df_out2), colnames(df))
    expect_equal(colnames(df_out2), colnames(df_out))

})

