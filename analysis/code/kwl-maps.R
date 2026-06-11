install.packages("magick")
library(magick)

# Read two PNG images
img1 <- image_read(here::here("analysis/figures/KWL_sample_units.png"))
img2 <- image_read(here::here("analysis/figures/P065WW-label.png"))

img1_scaled <- image_resize(img1, "2170")
img2_scaled <- image_resize(img2, "1800")

# Join them into an image vector
img_vector <- c(img1_scaled, img2_scaled)

# Combine side-by-side
side_by_side <- image_append(img_vector, stack = FALSE)
image_write(side_by_side, here::here("analysis", "figures", "map-strati-combined.png"))
