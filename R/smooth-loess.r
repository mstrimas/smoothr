# smooth_ksmooth <- function(x, wrap = FALSE, smoothness = 0.75, n = 10L,
#                            max_distance) {
#   stopifnot(is.matrix(x), ncol(x) == 2, nrow(x) > 1)
#   stopifnot(is_flag(wrap))
#   stopifnot(is.numeric(smoothness), length(smoothness) == 1, smoothness > 0)
#
#   # first densify
#   x <- smooth_densify(x, wrap = wrap, n = n), max_distance = max_distance)
#   n_pts <- nrow(x)
#   stopifnot(smoothness > (3 / n_pts))
#
#   # distance along curve
#   d <- c(0, cumsum(point_distance(x)))
#
#   # wrap vertices as necessary
#   if (wrap) {
#     x <- rbind(x[-n_pts, ], x, x[-1, ])
#     d <- c(d[1:(n_pts - 1)], d + d[n_pts], d[2:n_pts] + 2 *  d[n_pts])
#     # parameterize x and y as functions of distance along curve
#     # fit models
#     x_l <- stats::loess(y ~ x, span = smoothness,
#                         data = data.frame(x = d, y = x[, 1]))
#     y_l <- stats::loess(y ~ x, span = smoothness,
#                         data = data.frame(x = d, y = x[, 2]))
#     # predict models
#     d_pred <- data.frame(x = d[n_pts:(2 * n_pts - 1)])
#     x_l <- stats::predict(x_l, newdata = d_pred)
#     y_l <- stats::predict(y_l, newdata = d_pred)
#     # put coords back together
#     x <- cbind(x_l, y_l)
#     x <- x[complete.cases(x), ]
#     # ensure the endpoints match
#     x[nrow(x), ] <- x[1, ]
#   } else {
#     # parameterize x and y as functions of distance along curve
#     # fit models
#     x_l <- stats::loess(y ~ x, span = smoothness,
#                         data = data.frame(x = d, y = x[, 1]))
#     y_l <- stats::loess(y ~ x, span = smoothness,
#                         data = data.frame(x = d, y = x[, 2]))
#     # predict models
#     x_l <- stats::predict(x_l)
#     y_l <- stats::predict(y_l)
#     # put coords back together
#     x <- cbind(x_smooth, y_smooth)
#     x <- x[complete.cases(x), ]
#   }
#   return(x)
# }
#
# #
# # plot(jagged_lines$geometry[[2]][], type = "l")
# # xy <- as.data.frame(jagged_lines$geometry[[2]][])
# # names(xy) <- c("x", "y")
# # xy_p <- predict(stats::loess(y ~ x, data = xy, span = 1))
# # lines(xy$x, xy_p, col = "red")
