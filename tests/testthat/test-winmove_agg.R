context("winmove_agg")

test_that("shei calculation is correct", {
  d = winmove_agg(grainchanger::g_sf, 
                  grainchanger::cat_ls, 
                  5, "rectangle", "shei", lc_class = 0:3)
  expect_equal(d, grainchanger::wm_agg_shei)
})

test_that("mean calculation is correct", {
  d = winmove_agg(grainchanger::g_sf, 
                  grainchanger::cont_ls, 
                  5, "rectangle", "mean")
  expect_equal(d, grainchanger::wm_agg_mean)
})


test_that("output is vector of length of input", {
  d = winmove_agg(grainchanger::g_sf, 
                  grainchanger::cont_ls, 
                  5, "rectangle", "mean")
  expect_is(d, "numeric")
  expect_true(length(d) == nrow(grainchanger::g_sf))
})

test_that("winmove_agg can take different grid inputs", {
  expect_is(winmove_agg(grainchanger::g_sf, 
                        grainchanger::cont_ls, 
                        5, "rectangle", "mean"), "numeric")
  expect_is(winmove_agg(grainchanger::g_sp, 
                        grainchanger::cont_ls, 
                        5, "rectangle", "mean"), "numeric")
  expect_is(winmove_agg(grainchanger::g_raster, 
                        grainchanger::cont_ls, 
                        5, "rectangle", "mean"), "numeric")
})