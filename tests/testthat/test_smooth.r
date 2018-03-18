context("smooth()")
library(sf)

test_that("smooth() methods work", {
  # polygons
  s <- smooth(jagged_polygons[1:6, ], method = "spline")
  # change precision to fix some floating point issues on windows
  s <- st_set_precision(s, 1e6)
  expect_true(all(st_is_valid(s)))
  s <- smooth(jagged_polygons, method = "chaikin")
  # change precision to fix some floating point issues on windows
  s <- st_set_precision(s, 1e6)
  expect_true(all(st_is_valid(s)))

  # lines
  sl <- smooth(jagged_lines, method = "spline")
  expect_true(all(st_is_valid(s)))
  sl <- smooth(jagged_lines, method = "chaikin")
  expect_true(all(st_is_valid(s)))

  # test parameters
  # chaikin
  p <- jagged_polygons$geometry[[3]]
  s_r2 <- smooth(p, method = "chaikin", refinements = 1)
  s_r5 <- smooth(p, method = "chaikin", refinements = 5)
  expect_lt(nrow(s_r2[[1]]), nrow(s_r5[[1]]))
  # spline
  s_n100 <- smooth(p, method = "spline", n = 100)
  s_n1000 <- smooth(p, method = "spline", n = 1000)
  expect_lt(nrow(s_n100[[1]]), nrow(s_n1000[[1]]))
  expect_equal(nrow(s_n100[[1]]), 100)
  expect_equal(nrow(s_n1000[[1]]), 1000)
  s_vf2 <- smooth(p, method = "spline", vertex_factor = 2)
  s_vf4 <- smooth(p, method = "spline", vertex_factor = 4)
  expect_equal(nrow(s_vf2[[1]]), nrow(p[[1]]) * 2)
  expect_equal(nrow(s_vf4[[1]]), nrow(p[[1]]) * 4)
})

test_that("smooth() works for different input formats", {
  s_sf <- smooth(jagged_polygons)
  s_sfc <- smooth(st_geometry(jagged_polygons))
  s_sfg <- smooth(st_geometry(jagged_polygons)[[1]])
  s_spdf <- smooth(as(jagged_polygons, "Spatial"))
  s_sp <- smooth(as(as(jagged_polygons, "Spatial"), "SpatialPolygons"))
  expect_s3_class(s_sf, "sf")
  expect_s3_class(s_sfc, "sfc")
  expect_s3_class(s_sfg, "POLYGON")
  expect_s4_class(s_spdf, "SpatialPolygonsDataFrame")
  expect_s4_class(s_sp, "SpatialPolygons")
  expect_equal(nrow(s_sf), length(s_sfc))
  expect_equal(nrow(s_sf), length(s_spdf))
  expect_equivalent(st_set_geometry(s_sf, NULL), s_spdf@data)
})

test_that("smooth() preserves holes", {
  p <- jagged_polygons$geometry[[5]]
  expect_true(st_is_valid(smooth(p, method = "chaikin")))
  expect_true(st_is_valid(smooth(p, method = "spline")))
  expect_equal(length(p), length(smooth(p)))
})

test_that("smooth() preserves multipart features", {
  p <- jagged_polygons$geometry[[7]]
  expect_true(st_is_valid(smooth(p, method = "chaikin")))
  expect_true(st_is_valid(smooth(p, method = "spline")))
  expect_equal(length(p), length(smooth(p)))

  l <- jagged_lines$geometry[[8]]
  expect_true(st_is_valid(smooth(l, method = "chaikin")))
  expect_true(st_is_valid(smooth(l, method = "spline")))
  expect_equal(length(l), length(smooth(l)))
})

test_that("smooth() fails for points", {
  point <- st_point(c(0, 0))
  expect_error(smooth(point))
  expect_error(smooth(st_sfc(point)))
  expect_error(smooth(as(st_sfc(point), "Spatial")))
})
