library(tidyverse)
library(sf)
library(smoothr)
library(hexSticker)

p <- st_geometry(jagged_polygons[c(2, 4), ])
l <- st_geometry(jagged_lines[c(2, 7), ])
p_smooth <- smooth(p, method = "ksmooth", smoothness = 1)
l_smooth <- smooth(l, method = "ksmooth", smoothness = 1)

e <- expression({
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), mfrow = c(1, 2))
  plot(l[1], col = "grey30", lwd = 1, border = NA)
  plot(l_smooth[1], col = "#E41A1C", lwd = 1, add = TRUE)
  par(mar = c(0, 0, 0, 0))
  plot(p[2], col = "grey30", border = NA)
  plot(p_smooth[2], col = "#E41A1CAA", border = "#E41A1C", lwd = 0.5, add = TRUE)
})

sysfonts::font_add("Roboto", "/Library/Fonts/RobotoCondensed-Regular.ttf")
sticker(e, package = "smoothr", filename = "hex-logo/smoothr.png",
        p_size = 8, p_y = 0.5, p_color = "white", p_family = "Roboto",
        s_x = 1.02, s_y = 1.1, s_width = 1.5, s_height = 2,
        h_fill = "#7ac3ff", h_color = "#377eb8")
