context("plate_map")

data_96 <- data.frame(well = num_to_well(1:96),
		      val = rnorm(96, 100))

data_384 <- data.frame(well = num_to_well(1:384, plate = 384),
		       val = rnorm(384, 100))

out_96 <- plate_map(data = data_96$val, well = data_96$well)
out_384 <- plate_map(data = data_384$val, well = data_384$well)

test_that("plate_map returns a dataframe",{
    expect_true(is.data.frame(out_96))
    expect_equal(nrow(out_96), 96L)
    expect_equal(ncol(out_96), 4L)

    expect_true(is.data.frame(out_384))
    expect_equal(nrow(out_384), 384L)
    expect_equal(ncol(out_384), 4L)
})

test_that("plate_map returns expected columns",{
    expect_equal(names(out_96), c("well", "Row", "Column", "values"))
    expect_equal(names(out_384), c("well", "Row", "Column", "values"))
})

test_that("plate_map returns expected values",{
    expect_equal(as.character(out_96$well), num_to_well(1:96))
    expect_equal(as.character(out_384$well), num_to_well(1:384, plate = 384))

    expect_equal(out_96$Row, rep(1:8, each = 12))
    expect_equal(out_96$Column, rep(1:12, 8))
    expect_equal(out_96$values, data_96$val)

    expect_equal(out_384$Row, rep(1:16, each = 24))
    expect_equal(out_384$Column, rep(1:24, 16))
    expect_equal(out_384$values, data_384$val)
})


test_that("plate_map_scale returns expected values",{
    out_scale <- plate_map_scale(data = data_96$val,
				 well = data_96$well)

    expect_equal(mean(out_scale$values), 0, tolerance = 1e-5)
})



data_grid <- rbind(data_96, data_96)
data_grid$plate_id <- rep(c("plate_1", "plate_2"), each = 96)
out_grid <- plate_map_grid(data = data_grid$val,
			   well = data_grid$well,
			   plate_id = data_grid$plate_id)

test_that("plate_map_grid returns column of plate ids",{
    expect_equal(names(out_grid),
		 c("well", "Row", "Column", "values", "plate_label"))
})
