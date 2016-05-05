context("rnormalise")

# create data
N_PLATES <- 5
wells <- rep(num_to_well(1:96), N_PLATES)
plate_id <- rep(c("plate_1", "plate_2", "plate_3", "plate_4", "plate_5"),
		each = 96)
val1 <- rnorm(96 * N_PLATES, 10, 10)
val2 <- rnorm(96 * N_PLATES, 1, 100)
comps <- c(rep("cmpd", 80), rep("DMSO", 16))
compound <- rep(comps, N_PLATES)
df <- data.frame(Metadata_well = wells,
		 Metadata_plate_id = plate_id,
		 Metadata_compound = compound,
		 val1, val2)

test_that("returns errors when expected", {
    expect_error(r_normalise(iris))
    # NSE check
    expect_error(r_normalise(df,
			     plate_id = Metadata_plate_id,
			     compound = Metadata_compound,
			     neg_compound = "DMSO"))
    expect_error(r_normalise(df,
			     plate_id = "Metadata_plate_id",
			     compound = "Metadata_compound",
			     neg_compound = "not a compound"))

})

test_that("returns a dataframe", {
    df_out <- r_normalise(df,
			  plate_id = "Metadata_plate_id",
			  compound = "Metadata_compound",
			  neg_compound = "DMSO")
    expect_is(df_out, 'data.frame')
    expect_equal(ncol(df) == ncol(df_out), tolerance = 1e-7)
    expect_equal(nrow(df) == nrow(df_out), tolerance = 1e-7)
    expect_equal(colnames(df) == colnames(df_out))
})

