context("winmove")

test_that("shei calculation is correct", {
  d = winmove(grainchanger::cat_ls, 
                  5, "rectangle", "shei", lc_class = 0:3)
  expect_equal(d, grainchanger::wm_shei_dat)
})

test_that("mean calculation is correct", {
  d = winmove(grainchanger::cont_ls, 
                  5, "rectangle", "mean")
  expect_equal(d, grainchanger::wm_mean_dat)
})


test_that("output is raster of same resolution as input", {
  d = winmove(grainchanger::cont_ls, 
                  5, "rectangle", "mean")
  expect_is(d, "RasterLayer")
  expect_true(raster::ncell(d) == raster::ncell(grainchanger::cont_ls))
})