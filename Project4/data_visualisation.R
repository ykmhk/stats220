library(tidyverse)
logged_data<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQx8RQ1HSkYEoC9X9hIE9GFlQkFdEV8qBfjgls9OaSmtD3y4CXQFwzroQGAAk7RAEFPUArnnZghgaiQ/pub?output=csv")

glimpse(logged_data)

new_logged_data <- logged_data %>%
  select(-Timestamp) %>%
  rename(
    time_when_hungry = 1,
    time_of_last_meal = 2,
    food_type = 3,
    portion = 4,
    fullness = 5,
    activity = 6,
    activity_level = 7
  )%>%
  mutate(
    minutes_used_to_feel_hungry_again =
      as.numeric(
        difftime(
          # A tempoprary dummy date is added because difftime() requires complete datetime values, not just times
          ymd_hms(paste("2025-01-01", time_when_hungry)),
          ymd_hms(paste("2025-01-01", time_of_last_meal)),  
          units = "mins"
        )
      ),
   did_moderate_exercise = 
     ifelse(
       str_detect(str_to_lower(activity_level),"moderate"),
       "Yes","No"
       ),
   hour = as.numeric(str_sub(time_when_hungry, 1, 2)),
   hungry_period = case_when(
     hour < 12 ~ "Morning",
     hour < 17 ~ "Afternoon",
     hour < 21 ~ "Evening",
     TRUE ~ "Night"
    )
  )

# used in visual_data_story
# hungry_counts_total <- new_logged_data %>%
#   count(hungry_period)
# highest_frequncy_time <- hungry_counts_total %>%
#   filter(n == max(n)) %>%
#     pull(hungry_period)


# 1. food_vs_time_plot

food_vs_time_plot <- new_logged_data %>%
  ggplot(aes(
    y = reorder(food_type, minutes_used_to_feel_hungry_again),
    x = minutes_used_to_feel_hungry_again
  )) +
  
  geom_boxplot(
    aes(colour = food_type),
    fill = NA   #"transparent" was being treated like a category insted of a style so i used NA instead.
  ) +
  
  scale_colour_manual(values = c(
    "Fruits & light foods" = "#b8e024",
    "Snacks" = "#fedb78",
    "Main meal" = "#fba632",
    "Fast food" = "#e46f69"
  )) +
  
  geom_jitter(
    aes(fill = fullness),
    shape = 21,
    size= 2,
    colour = "#fec44f",
    height = 0.2,
    alpha = 0.7
  ) +
  
  scale_fill_gradient(
    low = "#ffffd4",
    high = "#d95f0e"
  )+
  
  scale_x_continuous(
    breaks = seq(
      0,
      max(new_logged_data$minutes_used_to_feel_hungry_again, na.rm = TRUE),
      20
    )
  ) +
  
  labs(
    title = "How long different food types sustain fullness",
    x = "Minutes it takes for me to feel hungry again",
    y = "Food type"
  ) +
  
  theme_minimal()

food_vs_time_plot

ggsave("plot3.png",food_vs_time_plot, width = 9, height = 5)
  


# 2. portion_size_vs_fullness_duration_plot

exercise <- new_logged_data %>%
  group_by(did_moderate_exercise)%>%
  summarise(avg_min_used_to_feel_hungry = round(mean(minutes_used_to_feel_hungry_again),0),
            mean_portion_i_had = round(mean(portion),0),
            n=n())
exercise

portion_size_vs_fullness_duration <- ggplot() +
  
  geom_jitter(data = new_logged_data,
    aes(y = portion,
        x = minutes_used_to_feel_hungry_again,
        colour = did_moderate_exercise),
    shape = 21,
    size= 2.5,
    height = 0.25,
  ) +
  
  geom_point(data = exercise,
             aes(y= mean_portion_i_had,
                 x = avg_min_used_to_feel_hungry,
                 colour = did_moderate_exercise),
             size= 3.5,
             shape=18)+
  
  scale_colour_manual(values = c(
    "Yes" = "#A5C920",
    "No" = "#fba632"
  )) +
  
  labs(
    title = "Portion size vs fullness duration",
    y = "Portion size",
    x = "Minutes it takes for me to feel hungry again",
    colour = "Whether I did moderate exercise"
  ) +
  
  theme_minimal()

portion_size_vs_fullness_duration


ggsave("plot2.png",portion_size_vs_fullness_duration, width = 8, height = 5)



# 3. frequency_of_hungry_during_difftimes

new_logged_data$hungry_period <- factor(
  new_logged_data$hungry_period,
  levels = c("Morning", "Afternoon", "Evening", "Night")
)

hungry_counts<-new_logged_data%>%
  count(hungry_period, portion)
hungry_counts

frequency_of_hungry_during_difftimes <- hungry_counts%>%
  ggplot(
    aes(x = hungry_period,
        y = n,
        fill = factor(portion))) +
  
  geom_col(position="dodge",
           alpha = 0.8) +
  
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    colour = "#91cf60",
    size = 4
  ) +
  
  scale_fill_manual(values = c(
    "1" = "#fee8c8",
    "2" = "#fdbb84",
    "3" = "#fba632",
    "4" = "#e34a33",
    "5" = "#A22451"
  )) +
  
  labs(
    title = "Frequency of feeling hungry at different times of day",
    x = "Time of day",
    y = "Frequency of feeling hungry",
    fill = "the protion of my last meal"
  ) +
  
  theme_minimal()

frequency_of_hungry_during_difftimes

ggsave("plot1.png",frequency_of_hungry_during_difftimes, width = 7, height = 5)

