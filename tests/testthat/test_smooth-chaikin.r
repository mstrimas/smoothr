context("smooth_chaikin()")

test_that("smooth_chaikin() works", {
  m <- jagged_polygons$geometry[[2]][[1]]
  m_smooth <- smooth_chaikin(m)
  expect_is(m_smooth, "matrix")
  expect_gt(nrow(m_smooth), nrow(m))
  expect_equal(m_smooth[1, ], m_smooth[nrow(m_smooth), ])
})

test_that("smooth_chaikin() works on lines", {
  l <- jagged_lines$geometry[[2]][]
  l_smooth <- smooth_chaikin(l, wrap = FALSE)
  expect_is(l_smooth, "matrix")
  expect_gt(nrow(l_smooth), nrow(l))
})

test_that("smooth_chaikin() raises error on invalid input", {
  expect_error(smooth_chaikin(jagged_polygons))
  m <- jagged_polygons$geometry[[2]][[1]]
  expect_error(smooth_chaikin(m, refinements = 100L))
  expect_error(smooth_chaikin(m, refinements = -1))
  expect_error(smooth_chaikin(m, refinements = 1.5))
})
