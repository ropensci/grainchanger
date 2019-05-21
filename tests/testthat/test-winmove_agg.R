context("winmove_agg")

test_that("shei calculation is correct", {
  d <- winmove_agg(coarse_dat = g_sf, 
                   fine_dat = cat_ls, 
                   d = 5, 
                   type = "rectangle", 
                   win_fun = "shei",
                   lc_class = 0:3)
  expect_equal(d, wm_agg_shei)
})

test_that("mean calculation is correct", {
  d <- winmove_agg(coarse_dat = g_sf, 
                   fine_dat = cont_ls, 
                   d = 5, 
                   type = "rectangle", 
                   win_fun = "mean")
  expect_equal(d, wm_agg_mean)
})


test_that("output is vector of length of input", {
  d <- winmove_agg(coarse_dat = g_sf, 
                   fine_dat = cont_ls, 
                   d = 5, 
                   type = "rectangle", 
                   win_fun = "mean")
  expect_is(d, "numeric")
  expect_true(length(d) == nrow(g_sf))
})

test_that("winmove_agg can take different grid inputs", {
  # sf object
  expect_is(winmove_agg(coarse_dat = g_sf, 
                        fine_dat = cont_ls, 
                        d = 10, 
                        type = "rectangle", 
                        win_fun = "mean",
                        agg_fun = "var"), 
            "numeric")
  # sp object
  expect_is(winmove_agg(coarse_dat = g_sp, 
                        fine_dat = cont_ls, 
                        d = 10, 
                        type = "rectangle", 
                        win_fun = "mean",
                        agg_fun = "var"), 
            "numeric")
  # raster object
  expect_is(winmove_agg(coarse_dat = g_raster, 
                        fine_dat = cont_ls, 
                        d = 10, 
                        type = "rectangle", 
                        win_fun = "mean",
                        agg_fun = "var"), "numeric")
})
