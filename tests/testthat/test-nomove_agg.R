context("nomove_agg")

test_that("shei calculation is correct", {
  d <- nomove_agg(coarse_dat = g_sf, 
                  fine_dat = cat_ls, 
                  agg_fun = shei, 
                  lc_class = 0:3)
  expect_equal(d, nm_agg_shei)
})

test_that("mean calculation is correct", {
  d <- nomove_agg(coarse_dat = g_raster, 
                  fine_dat = cont_ls, 
                  agg_fun = mean)
  expect_equal(d, nm_agg_mean)
})

test_that("prop calculation is correct", {
  d <- nomove_agg(coarse_dat = g_sp, 
                  fine_dat = cat_ls, 
                  agg_fun = prop,
                  lc_class = 1)
  expect_equal(d, nm_agg_prop)
})

test_that("range calculation is correct", {
  d <- nomove_agg(coarse_dat = poly_sf, 
                  fine_dat = cont_ls, 
                  agg_fun = var_range,
                  is_grid = FALSE)
  expect_equal(d, nm_agg_range)
})

test_that("can accept user defined function", {
  user_fn <- function(x, na.rm = TRUE) {
    sum(2*x)
  }
  
  d <- nomove_agg(coarse_dat = g_sp, 
                  fine_dat = cat_ls, 
                  agg_fun = user_fn)
  
  expect_equal(d, nm_agg_user)
})

test_that("output is vector of length of grid for sf input", {
  d <- nomove_agg(g_sf, cont_ls, mean)
  expect_is(d, "numeric")
  expect_true(length(d) == nrow(g_sf))
})

test_that("output is raster of resolution of grid for raster input", {
  d <- nomove_agg(g_raster, cont_ls, mean)
  expect_is(d, "RasterLayer")
  expect_true(raster::ncell(d) == raster::ncell(g_raster))
})


test_that("winmove_agg can take different grid inputs", {
  expect_is(nomove_agg(g_sf, cont_ls, mean), "numeric")
  expect_is(nomove_agg(g_sp, cont_ls, mean), "numeric")
  expect_is(nomove_agg(g_raster, cont_ls, mean), "RasterLayer")
})

