context("winmove_agg")

test_that("shei calculation is correct", {
  d <- winmove_agg(g_sf, cat_ls, 5, "rectangle", "shei", lc_class = 0:3)
  expect_equal(d, wm_agg_shei)
})

test_that("mean calculation is correct", {
  d <- winmove_agg(g_sf, cont_ls, 5, "rectangle", "mean")
  expect_equal(d, wm_agg_mean)
})


test_that("output is vector of length of input", {
  d <- winmove_agg(g_sf, cont_ls, 5, "rectangle", "mean")
  expect_is(d, "numeric")
  expect_true(length(d) == nrow(g_sf))
})

test_that("winmove_agg can take different grid inputs", {
  expect_is(winmove_agg(g_sf, cont_ls, 5, "rectangle", "mean"), "numeric")
  expect_is(winmove_agg(g_sp, cont_ls, 5, "rectangle", "mean"), "numeric")
  expect_is(winmove_agg(g_raster, cont_ls, 5, "rectangle", "mean"), "numeric")
})
