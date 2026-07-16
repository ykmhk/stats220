library(tidyverse)
logged_data<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTfA9NsfdrQ4xbZvvUFHITvjxyUXMEnAcc49f6RX6wL5T2YsvOluK6s_UTtdev-nhsA5iHlUXQ8Zmrx/pub?gid=1892366098&single=true&output=csv")
latest_data <- logged_data %>%
  select(-Timestamp) %>%
  rename(
    time_when_hungry = 1,
    food_type = 2,
    portion = 3,
    minutes_until_hungry = 4,
    fullness = 5,
    activity = 6,
    activity_level = 7
  )

glimpse(latest_data)

minimum_min_until_hungry <- min(latest_data$minutes_until_hungry, na.rm = TRUE)
maximum_min_until_hungry <- max(latest_data$minutes_until_hungry, na.rm = TRUE)
mean_min_until_hungry <- mean(latest_data$minutes_until_hungry, na.rm = TRUE)

minimum_hr_until_hungry <- (minimum_min_until_hungry/ 60) %>% round(1)
maximum_hr_until_hungry <- (maximum_min_until_hungry/ 60) %>% round(1)
mean_hr_until_hungry <- (mean_min_until_hungry/ 60) %>% round(1)

all_minutes_in_hour <- (latest_data$minutes_until_hungry / 60) %>% round(1)
number_of_response<- nrow(latest_data)
mean_fullness<-mean(latest_data$fullness, na.rm = TRUE)


#Summary values
minimum_min_until_hungry
minimum_hr_until_hungry
maximum_min_until_hungry
maximum_hr_until_hungry
mean_min_until_hungry
mean_hr_until_hungry

all_minutes_in_hour
number_of_response 
mean_fullness

#Plot for distribution of food types
latest_data %>%
  ggplot() +
  geom_bar(aes(x = food_type) ,fill ="#BDC9D0" ) +
  labs(
    title = "Distribution of Food Types I had",
    x = "Food Type",
    y = "Count"
  )

#Plot for distribution of food portion size
latest_data %>%
  ggplot() +
  geom_bar(aes(x = portion),fill = "#C1E7E4") +
  labs(
    title = "Distribution of food portion size I had",
    x = "Portion Size",
    y = "Count"
  )

#Plot for activity distribution across different minutes until feeling hungry
latest_data %>%
  ggplot() +
  geom_bar(aes(x = minutes_until_hungry ,fill =activity) ) +
  labs(
    title = "Activity occurrence relative to minutes until feeling hungry",
    x = "Minutes Pasted Until Hungry",
    y = "Count",
    fill = "Activity"
  )


#Integer values like 0, 1, 2, 3, 4, and 5 are treated by ggplot as continuous variables rather than categorical ones. As a result, ggplot doesn’t know how to group them properly, and the fill aesthetic may be ignored.
#After searching and learning, I found that using factor() can convert a variable from a continuous numeric type into a categorical variable (i.e., category labels). This allows ggplot to correctly group the data and apply colors using fill.
latest_data$fullness <- factor(latest_data$fullness)

#Plot for fullness distribution across different minutes until feeling hungry
latest_data %>%
  ggplot() +
  geom_bar(aes(x = minutes_until_hungry ,fill = fullness) ) +
  labs(
    title = "Fullness relative to minutes until feeling hungry",
    x = "Minutes Pasted Until Hungry",
    y = "Count",
    fill = "Fullness level"
  )


# ----------------------------------
# Final report code i will use

# Numeric summaries for report
minimum_min_until_hungry 
maximum_min_until_hungry
mean_min_until_hungry
minimum_hr_until_hungry
maximum_hr_until_hungry 
mean_hr_until_hungry
number_of_response
mean_fullness

# Bar charts for report:
# All 4 of them

