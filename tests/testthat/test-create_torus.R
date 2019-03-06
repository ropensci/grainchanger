context("create_torus")

test_that("torus output is correct", {
  d = create_torus(cat_ls, 5)
  expect_equal(d, torus)
})

