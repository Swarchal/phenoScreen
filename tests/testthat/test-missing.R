context("handle missing data")


# randomly replace values with NA's
create_na <- function (x, prop = 0.1) {
    if (is.data.frame(x)) {
        n = nrow(x)
        p = ncol(x)
    } else if (is.vector(x)) {
        n = length(x)
        p = 1
    }
    NAloc = rep(FALSE, times = n * p)
    NAloc[sample.int(n * p, floor(n * p * prop))] = TRUE
    x[matrix(NAloc, nrow = n, ncol = p)] = NA
    return(x)
}


# create example data
make_data <- function() {
    N_PLATES = 5
    wells = rep(platetools::num_to_well(1:96), N_PLATES)
    plate_id = rep(c("plate_1", "plate_2", "plate_3", "plate_4", "plate_5"),
                   each = 96)
    val1 = rnorm(96 * N_PLATES, 10, 10)
    val2 = rnorm(96 * N_PLATES, 1, 100)
    val3 = rnorm(96 * N_PLATES, 5, 10)
    val4 = rnorm(96 * N_PLATES, 20, 0.1)
    comps = c(rep("cmpd", 80), rep("DMSO", 16))
    compound = rep(comps, N_PLATES)

    df = data.frame(Metadata_well = wells,
            Metadata_plate_id = plate_id,
            Metadata_compound = compound,
            val1, val2)

    df2 = cbind(df, val3, val4)
    return(df2)
}


df2 = make_data()

# create dataframe with one column (val1) containing 90% missing data
df2_m = df2
df2_m[["val1"]] = create_na(df2_m[["val1"]], 0.5)

test_that("drop_missing_cols removes column", {
    output = drop_missing_cols(df2_m, threshold=0.4)
    expect_true(! "val1" %in% colnames(output))
})

test_that("drop_missing_cols respects threshold value", {
    output = drop_missing_cols(df2_m, threshold=0.99)
    expect_equal(colnames(output), colnames(df2_m))
})


test_that("drop_missing_rows removes rows", {
    output = drop_missing_rows(df2_m, threshold=0.8)
    expect_lt(nrow(output), nrow(df2_m)) # expect_less_than hadley!
    expect_true(all(complete.cases(output)))
})


test_that("impute_missing errors with invalid method", {
    expect_error(impute_missing(df2_m, method="invalid"))
})


test_that("impute_missing mean returns correct value", {
    test_df =  data.frame(
        x = c(1L, 2L, 3L, 400L, NA),
        y = c(100L, 200L, 300L, 4000L, NA),
        Metadata_col = c(rep("string", 4), NA)
    )
    output = impute_missing(test_df, method = "mean")
    expect_equal(as.numeric(output[nrow(output), "x"]), 101.5)
    expect_equal(as.numeric(output[nrow(output), "y"]), 1150)
    # check it's not imputed metadata columns
    expect_equal(test_df[["Metadata_col"]], output[["Metadata_col"]])
})


test_that("impute_missing median returns correct value", {
    test_df =  data.frame(
        x = c(1L, 2L, 3L, 400L, NA),
        y = c(100L, 200L, 300L, 4000L, NA),
        Metadata_col = c(rep("string", 4), NA)
    )
    output = impute_missing(test_df, method = "median")
    expect_equal(as.numeric(output[nrow(output), "x"]), 2.5)
    expect_equal(as.numeric(output[nrow(output), "y"]), 250)
    # check it's not imputed metadata columns
    expect_equal(test_df[["Metadata_col"]], output[["Metadata_col"]])
})


test_that("impute_missing works with grouped dataframe", {
    # TODO: write test
    expect_true(TRUE)
})


test_that("impute_missing nn returns correct value", {
    # TODO: write test
    expect_true(TRUE)
})