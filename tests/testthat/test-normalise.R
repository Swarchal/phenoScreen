 context("normalise")
 
# create example data
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

df_out <- normalise(df,
		    plate_id = "Metadata_plate_id",
		    compound = "Metadata_compound",
		    neg_compound = "DMSO")

test_that("errors when expected",{
    expect_error(normalise(val1, plate_id, compound, neg_compound = "DMSO"))
    expect_error(normalise(df, "plate_id", compound, "DMSO"))
})

test_that("returns a dataframe",{
    expect_is(df_out, "data.frame")
    expect_equal(nrow(df_out), N_PLATES * 96)
})

df_out_sub <- normalise(df,
			plate_id = "Metadata_plate_id",
			compound = "Metadata_compound",
			neg_compound = "DMSO",
			method = "subtract")

test_that("subtract returns a dataframe",{
    expect_is(df_out_sub, "data.frame")
})



test_that("divide and subtract return different answers",{
    expect_true(any(df_out_sub != df_out))
})

test_that("returns error for incorrect method",{
    expect_error(
		 normalise(df,
			   plate_id = "Metadata_plate_id",
			   compound = "Metadata_compound",
			   neg_compound = "DMSO",
			   method = "not_valid_method")
		 )
})
