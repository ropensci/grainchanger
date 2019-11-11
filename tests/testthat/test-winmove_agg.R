context("winmove_agg")

test_that("shei calculation is correct", {
  d <- winmove_agg(coarse_dat = g_sf, 
                   fine_dat = cat_ls, 
                   d = 12, 
                   type = "rectangle", 
                   win_fun = shei,
                   lc_class = 1:4, 
                   quiet = TRUE)
  expect_equal(d, wm_agg_shei)
})

test_that("shdi calculation is correct", {
  d <- winmove_agg(coarse_dat = g_sf, 
                   fine_dat = cat_ls, 
                   d = 3, 
                   type = "circle", 
                   win_fun = shdi,
                   lc_class = 1:4, 
                   quiet = TRUE)
  expect_equal(d, wm_agg_shdi)
})

test_that("prop calculation is correct when lc given does not exist", {
  d <- winmove_agg(coarse_dat = g_sf, 
                   fine_dat = cat_ls, 
                   d = 5, 
                   type = "rectangle", 
                   win_fun = prop,
                   lc_class = 10, 
                   quiet = TRUE)
  expect_true(all(d == 0))
})

test_that("mean calculation with non-rectangle coarse data is correct", {
  d <- winmove_agg(coarse_dat = poly_sf, 
                   fine_dat = cont_ls, 
                   d = 4, 
                   type = "rectangle", 
                   win_fun = mean,
                   agg_fun = var,
                   is_grid = FALSE, 
                   quiet = TRUE)
  expect_equal(d, wm_agg_mean)
})

test_that("output is vector of length of input", {
  d <- winmove_agg(coarse_dat = g_sf, 
                   fine_dat = cont_ls, 
                   d = 5, 
                   type = "circle", 
                   win_fun = mean, 
                   quiet = TRUE)
  expect_is(d, "numeric")
  expect_true(length(d) == nrow(g_sf))
})

test_that("throws warning about edge effects", {
  expect_warning(winmove_agg(coarse_dat = g_sf, 
                   fine_dat = cat_ls, 
                   d = 12, 
                   type = "rectangle", 
                   win_fun = shei,
                   lc_class = 1:4),
                 "Moving window extends beyond extent of `fine_dat`")
})

test_that("throws warning about edge effects", {
  expect_output(winmove_agg(coarse_dat = g_sf, 
                             fine_dat = cat_ls, 
                             d = 3, 
                             type = "rectangle", 
                             win_fun = prop,
                             lc_class = 1, 
                            quiet = TRUE),
                 "aggregation assumes all cells are rectangular")
})


test_that("winmove_agg can take different grid inputs", {
  # sf object
  expect_is(winmove_agg(coarse_dat = g_sf, 
                        fine_dat = cont_ls, 
                        d = 10, 
                        type = "rectangle", 
                        win_fun = mean,
                        agg_fun = var, 
                        quiet = TRUE), 
            "numeric")
  # sp object
  expect_is(winmove_agg(coarse_dat = g_sp, 
                        fine_dat = cont_ls, 
                        d = 10, 
                        type = "rectangle", 
                        win_fun = mean,
                        agg_fun = var, 
                        quiet = TRUE), 
            "numeric")
  # raster object
  expect_is(winmove_agg(coarse_dat = g_raster, 
                        fine_dat = cont_ls, 
                        d = 8, 
                        type = "circle", 
                        win_fun = mean,
                        agg_fun = var, 
                        quiet = TRUE), "RasterLayer")
})
