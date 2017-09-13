context("transformations")
set.seed(12321)

test_that("glog returns numbers", {
    x <- rnorm(1000)
    out <- glog(x)
    expect_equal(length(x), length(out))
    expect_true(is.numeric(out))
})

test_that("glog returns a matrix", {
    mat <- matrix(rnorm(10000), ncol = 100)
    mat_out <- glog(mat)
    expect_true(is.matrix(mat_out))
    expect_equal(ncol(mat_out), 100)
})

test_that("glog roughly normalises data", {
    # create a skewed distribution
    a <- rnorm(1000)^2
    # w value from shapiro wilk test, high = normal distribution
    w_a <- shapiro.test(a)$statistic
    # glog transformation and w value
    glog_a <- glog(a)
    w_glog_a <- shapiro.test(glog_a)$statistic
    # check transformation if more normal than original data
    expect_true(w_a < w_glog_a)
})



# create example data
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



out = pca(df)

test_that("pca returns a dataframe", {
    expect_is(out, "data.frame")
})


test_that("pca retains metadata", {
    orig_metadata_columns = get_metadata_cols(df)
    pca_metadata_columns = get_metadata_cols(out)
    expect_equal(orig_metadata_columns, pca_metadata_columns)
})


test_that("pca n_components works", {
    out_new = pca(df, n_components=1L)
    pc_colnames = get_feature_cols(out_new)
    expect_equal(pc_colnames, "PC1")
})


test_that("pca errors if n_components > num features", {
    expect_error(pca(df, n_components=1000))
})


test_that("pca n_components based on variance", {
    out_0.3 = pca(df2, n_components = 0.3)
    expect_is(out, "data.frame")
    expect_true(ncol(out) > ncol(out_0.3))
    expect_error(pca(df2, n_components = -12))
})