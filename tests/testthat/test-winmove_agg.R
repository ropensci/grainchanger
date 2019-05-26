context("winmove_agg")

test_that("shei calculation is correct", {
  d <- winmove_agg(coarse_dat = g_sf, 
                   fine_dat = cat_ls, 
                   d = 20, 
                   type = "rectangle", 
                   win_fun = shei,
                   lc_class = 1:4)
  expect_equal(d, wm_agg_shei)
})

test_that("shdi calculation is correct", {
  d <- winmove_agg(coarse_dat = g_sf, 
                   fine_dat = cat_ls, 
                   d = 3, 
                   type = "Gauss", 
                   win_fun = shdi,
                   lc_class = 1:4)
  expect_equal(d, wm_agg_shdi)
})

test_that("var_range calculation is correct", {
  d <- winmove_agg(coarse_dat = poly_sf, 
                   fine_dat = cont_ls, 
                   d = 3, 
                   type = "rectangle", 
                   win_fun = var_range)
  expect_equal(d, wm_agg_range)
})

test_that("prop calculation is correct when lc given does not exist", {
  d <- winmove_agg(coarse_dat = g_sf, 
                   fine_dat = cat_ls, 
                   d = 5, 
                   type = "rectangle", 
                   win_fun = prop,
                   lc_class = 10)
  expect_true(all(d == 0))
})

test_that("mean calculation with non-rectangle coarse data is correct", {
  d <- winmove_agg(coarse_dat = poly_sf, 
                   fine_dat = cont_ls, 
                   d = 4, 
                   type = "rectangle", 
                   win_fun = mean,
                   agg_fun = var,
                   is_grid = FALSE)
  expect_equal(d, wm_agg_mean)
})


test_that("mean calculation is correct with na.rm = TRUE", {
  d <- winmove_agg(coarse_dat = g_raster, 
                   fine_dat = cont_ls, 
                   d = 10, 
                   type = "circle", 
                   win_fun = mean,
                   agg_fun = sd,
                   na.rm = TRUE)
  expect_equal(d, wm_agg_mean_na)
})


test_that("output is vector of length of input", {
  d <- winmove_agg(coarse_dat = g_sf, 
                   fine_dat = cont_ls, 
                   d = 5, 
                   type = "Gauss", 
                   win_fun = mean)
  expect_is(d, "numeric")
  expect_true(length(d) == nrow(g_sf))
})

test_that("winmove_agg can take different grid inputs", {
  # sf object
  expect_is(winmove_agg(coarse_dat = g_sf, 
                        fine_dat = cont_ls, 
                        d = 10, 
                        type = "rectangle", 
                        win_fun = mean,
                        agg_fun = var), 
            "numeric")
  # sp object
  expect_is(winmove_agg(coarse_dat = g_sp, 
                        fine_dat = cont_ls, 
                        d = 10, 
                        type = "rectangle", 
                        win_fun = mean,
                        agg_fun = var), 
            "numeric")
  # raster object
  expect_is(winmove_agg(coarse_dat = g_raster, 
                        fine_dat = cont_ls, 
                        d = 8, 
                        type = "circle", 
                        win_fun = mean,
                        agg_fun = var), "RasterLayer")
})
