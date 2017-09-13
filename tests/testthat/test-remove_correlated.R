context("remove_correlated")

# create example dataframe
mydata <- structure(
    list(V1 = c(1L, 2L, 5L, 4L, 366L, 65L, 43L, 456L, 876L,
                78L, 687L, 378L, 378L, 34L, 53L, 43L),
         V2 = c(2L, 2L, 5L, 4L, 366L, 65L, 43L, 456L, 876L, 78L, 687L,
                378L, 378L, 34L, 53L, 41L),
         V3 = c(10L, 20L, 10L, 20L, 10L, 20L, 1L, 0L, 1L, 2010L,
                20L, 10L, 10L, 10L, 10L, 10L),
         V4 = c(2L, 10L, 31L, 2L, 2L, 5L,
                2L, 5L, 1L, 52L, 1L, 2L, 52L, 6L, 2L, 1L),
         V5 = c(4L, 10L, 31L, 2L, 2L, 5L, 2L, 5L, 1L, 52L, 1L,
                2L, 52L, 6L, 2L, 3L)),
        .Names = c("V1", "V2", "V3", "V4", "V5"),
         class = "data.frame", row.names = c(NA, -16L))


metadata = data.frame(Metadata_compound = 1:16,
                      Metadata_other = 2:17)

mydata = cbind(mydata, metadata)

corr_cols = find_correlated(mydata, threshold = 0.95)

reduced_df = remove_correlated(mydata, threshold = 0.95)

test_that("remove_correlated returns a dataframe", {
     expect_is(reduced_df, "data.frame")
})

test_that("not removed any metadata columns", {
    ans = c("Metadata_compound", "Metadata_other") %in% colnames(reduced_df)
    expect_equal(ans, c(TRUE, TRUE))
})

