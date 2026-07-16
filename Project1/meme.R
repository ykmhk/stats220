library(magick)
library(tidyverse)

img <- image_read("inspo_meme.png")
# meme= img passed to two image_annotate to add text on
meme <- img %>%   
  image_annotate(
    "       Let's see if this fixes the two bugs        ",
    gravity = "center",
    location = "+0-50",    #moved up
    size = 45,
    color = "white",
    boxcolor = "black",
    font = "Impact",
  ) %>%     # image with both text layers applied
  image_annotate(
    " IT CREATED TWO MORE ",
    gravity = "south",
    location = "+0+45",
    size = 50,
    color = "white",
    boxcolor = "black",
    font = "Impact"
  )

meme

image_write(meme, "my_meme.png")


# get the image dimensions
info <- image_info(meme)
img_width <- info$width
img_height <- info$height

# split the image 
top_half <- meme %>%
  image_crop(geometry = geometry_area(width = img_width, height = img_height/2, ))

bottom_half <- meme %>%
  image_crop(geometry = geometry_area(width = img_width, height = img_height/2, y_off = img_height/2))

bottom_half_modulated <- bottom_half %>%image_modulate(brightness = 120, saturation = 80)

frame1 <- top_half
frame2 <- top_half
frame3 <- bottom_half_modulated
frame4 <- bottom_half_modulated 

# combine 
frames <- c(frame1, frame2, frame3, frame4)


animation <- image_animate(frames, fps = 1) 

# display
animation

image_write(animation, "my_animated_meme.gif")

