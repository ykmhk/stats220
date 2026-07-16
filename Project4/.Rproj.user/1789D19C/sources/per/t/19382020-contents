library(tidyverse)
library(httr)
library(magick)

api_key <- "VAA00I2wbzBGNfPkaaf3aqR38H9rzpFHv3Bd8tYXR2IEAzrTLgtNRCq5"

url <- "https://api.pexels.com/v1/search?query=relaxation%20spot&per_page=80"

response <- httr::GET(url, 
                      add_headers(Authorization = api_key))

data <- httr::content(response, 
                      as = "parsed", 
                      type = "application/json")

photo_data <- tibble(photos = data$photos) %>%
  unnest_wider(photos) %>%
  unnest_wider(src)


selected_photos<-photo_data%>%
  mutate(
    # Create category of horizontal or vertical photo by comparing width and height
    orientation = ifelse(width > height, "landscape", "portrait"),
    
    # Area of each photo
    area = width * height,
    
    # Group ave colors into simple categories based on hex values
    # Using first two letter in hex color (^ start with, #start of hex color,[range]of first char)
    # If start with digit 0-3 then it tends to be dark
    # else if start with char A-F tends to be light.
    # else is medium.
    color_group = ifelse(grepl("^#([0-3])", avg_color), "dark",
                         ifelse(grepl("^#([A-F])", avg_color), "light", "medium")),
    
    # Check if alt description contain key words cozy or peaceful
    include_word_cozy_or_peaceful = str_detect(str_to_lower(alt), "cozy|peaceful")
    
  ) %>%
    filter(orientation == "portrait") %>%
    slice(1:20)

write_csv(selected_photos, "selected_photos.csv")


# Summaries

# number of photos with average color as medium in color_group)
total_medium_color <- sum(selected_photos$color_group=="medium")
# number of photos with alt that contain word cozy or peaceful
total_alt_including_cozy_or_peaceful <-sum(selected_photos$include_word_cozy_or_peaceful)
# mean area of selected_photos
selected_mean_area<-mean(selected_photos$area)

total_medium_color
total_alt_including_cozy_or_peaceful
selected_mean_area

# grouped_photos with variables (mean_area,max_area,min_area,number of photos) for each group 
grouped_photos <- selected_photos %>%
  group_by(include_word_cozy_or_peaceful) %>%
  summarise(
    mean_area = mean(area),       
    max_area = max(area),        
    min_area = min(area),         
    count = n()                   
  )
grouped_photos

max_area_with_keywords <- grouped_photos$max_area[2]

# tried to use pull with filter selecting rows instead of writing grouped_photos$var[1]/[2]
# to avoid if not knowing exact index of true and false

mean_area_with_keywords<-grouped_photos %>%
  filter(include_word_cozy_or_peaceful == TRUE) %>%
  pull(mean_area)

count_without_keywords <- grouped_photos %>%
  filter(include_word_cozy_or_peaceful == FALSE) %>%
  pull(count)


max_area_with_keywords
mean_area_with_keywords
count_without_keywords

# creating meme
img <- image_read(selected_photos$large[9])
meme_without_effect <- img %>%   
  image_annotate("fo(r)est.",
                 size = 40,
                 gravity="center",
                 font = "serif",
                 color = "#FABDE8"
  )

meme_without_effect

# frame 1 (base meme)
f1 <- meme_without_effect

# frame 2 (add on sparkle right bottom)
f2 <- meme_without_effect %>%
  image_annotate("+",
                 size = 18,
                 gravity = "center",
                 location = "+67+20",
                 color = "#FABDE8")

# Frame 3 (add on sparkle left top)
f3 <- meme_without_effect %>%
  image_annotate("+",
                 size = 26,
                 gravity = "center",
                 location = "-70-20",
                 color = "#FABDE8")

# Frame 4 (add on both sparkles)
f4 <- meme_without_effect %>%
  image_annotate("+",
                 size = 18,
                 gravity = "center",
                 location = "+67+20",
                 color = "#FABDE8") %>%
  image_annotate("+",
                 size = 26,
                 gravity = "center",
                 location = "-70-20",
                 color = "#FABDE8")

# Combining frames
frames <- c(f1, f3, f2, f3, f4,f2,f3)
animation <- image_animate(frames, fps = 2)
animation
image_write(meme_without_effect,"creativity.png")
image_write(animation,"creativity.gif")

