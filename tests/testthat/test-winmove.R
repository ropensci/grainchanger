context("winmove")

test_that("shei calculation is correct", {
  d <- winmove(cat_ls, 5, "rectangle", shei, lc_class = 0:3)
  expect_equal(d, wm_shei_dat)
})

test_that("mean calculation is correct", {
  d <- winmove(cont_ls, 2, "Gauss", mean)
  expect_equal(d, wm_mean_dat)
})

test_that("mean na.rm = TRUE calculation is correct", {
  d <- winmove(cont_ls, 15, "circle", mean, na.rm = TRUE)
  expect_equal(d, wm_mean_na_dat)
})



test_that("output is raster of same resolution as input", {
  d <- winmove(cont_ls, 5, "rectangle", mean)
  expect_is(d, "RasterLayer")
  expect_true(raster::ncell(d) == raster::ncell(cont_ls))
})
