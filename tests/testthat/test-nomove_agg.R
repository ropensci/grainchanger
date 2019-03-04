context("nomove_agg")

test_that("shei calculation is correct", {
  d = nomove_agg(grainchanger::g_sf, 
                  grainchanger::cat_ls, 
                  "shei", lc_class = 0:3)
  expect_equal(d, grainchanger::nm_agg_shei)
})

test_that("mean calculation is correct", {
  d = nomove_agg(grainchanger::g_sf, 
                  grainchanger::cont_ls, 
                  "mean")
  expect_equal(d, grainchanger::nm_agg_mean)
})


test_that("output is vector of length of grid for sf", {
  d = nomove_agg(grainchanger::g_sf, 
                 grainchanger::cont_ls, 
                 "mean")
  expect_is(d, "numeric")
  expect_true(length(d) == nrow(grainchanger::g_sf))
})

test_that("output is raster of resolution of grid for raster input", {
  d = nomove_agg(grainchanger::g_raster, 
                 grainchanger::cont_ls, 
                 "mean")
  expect_is(d, "RasterLayer")
  expect_true(raster::ncell(d) == raster::ncell(grainchanger::g_raster))
})


test_that("winmove_agg can take different grid inputs", {
  expect_is(nomove_agg(grainchanger::g_sf, 
                        grainchanger::cont_ls, 
                        "mean"), "numeric")
  expect_is(nomove_agg(grainchanger::g_sp, 
                        grainchanger::cont_ls, 
                        "mean"), "numeric")
  expect_is(nomove_agg(grainchanger::g_raster, 
                        grainchanger::cont_ls, 
                        "mean"), "RasterLayer")
})
