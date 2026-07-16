library(tidyverse)
library(stringr)
beehivedata <- readRDS("beehive.rds")
ministersdata <- readRDS("ministers.rds")

beehive_clean <-beehivedata%>%
  separate_rows(ministers,sep=";")

ministers_clean <- ministersdata %>%
  select(minister, role) %>%
  group_by(minister) %>%
  summarise(minister_roles = paste(unique(role), collapse = ", ")) #from 2A
ministers_clean

combind_data<-left_join(beehive_clean,ministers_clean,by=c("ministers"="minister"))

#Analyse whether Beehive releases from the Social Development and Employment portfolio 
#focus more on employment-related issues or welfare/social support issues.
combind_data <- combind_data %>%
  mutate(
    title_lower = str_to_lower(title),
    summary_lower = str_to_lower(summary),
    year = str_sub(date_text, -4, -1),
    category = case_when(
      
      # Titles are prioritised because they usually reflect the main topic of the release. 
      # Default assumption Welfare as 
      # generally if these Employment key word didn't show up 
      # it would be more welfare/social development focused. 
      
      str_detect(title_lower,"employment|employer|unemployed|job|work") ~ "Employment",
      str_detect(title_lower,"welfare|financial|homeowners|living|rent|children|support") ~ "Welfare",
      
      # Employment in summary (only if title is not Welfare or Employment)
      str_detect(summary_lower,"employment|employer|unemployed|job|work") ~ "Employment",
      #default
      TRUE ~ "Welfare"
    ),
    
    subcategory = case_when(
      # Employment
      category == "Employment" ~ "Employment",
      
      # subcategory for welfare
      str_detect(title_lower, "financial|homeowners|living|rent") | str_detect(summary_lower, "financial|homeowner|living|rent")
      ~ "Financial/Housing",
      
      str_detect(title_lower, "children|support") | str_detect(summary_lower, "children|support")
      ~ "Other Supporting",
      
      TRUE ~ "General Social Development"
    )
  )

# summerised table for making plots
plot_data <- combind_data %>%
  count(ministers, category,subcategory,minister_roles,year)

# exploring plots 1&2
plot1<-ggplot(plot_data,
       aes(y = ministers,
           x = n,
           fill = category)) +
  geom_col(position = "dodge")+
  labs(x="number of releases",
       y="ministers",
       title="Employment vs Welfare focus within Beehive releases from Social Development & Employment portfolio")
plot1

#trying facet_wrap
plot2<-ggplot(plot_data) +
  geom_col(aes(x = category, y = n, fill = category)) +
  facet_wrap(~ ministers)
plot2



# decided to make visualisation focuses on 
# the primary minister of the Social Development and Employment portfolio, 
# as they are responsible for the majority of relevant releases,
# avoiding distortion from ministers with only occasional responsibilities

primary_minister <- combind_data %>%
  count(ministers) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(ministers)

# group by year to count percentage of each category's number out of number of releases for each year.
plot_year <- combind_data %>%
  filter(ministers == primary_minister) %>%
  count(year, category, subcategory,minister_roles) %>%
  group_by(year) %>%
  mutate(prop = n / sum(n))


# cleaning up role descriptions to add in to plot
role_text<- plot_year$minister_roles%>%
  str_replace_all(", ", "\n")%>%
  str_remove("Personal details")

plot3<-ggplot(plot_year, aes(x = category, y = n, fill = subcategory)) +
  geom_col() +
  
  facet_wrap(~ year) +
  
  geom_text(
    aes(label = paste0(n, "  (", round(prop * 100, 1), "%)")),
    position = position_stack(vjust = 0.5),
    colour = "white",
    size = 3
  ) +
  
  scale_y_continuous(breaks = seq(0, max(plot_year$n), by = 10))+
  
  scale_fill_manual(values = c(
    "Other Supporting" = "#b8e024",
    "Financial/Housing" = "#AB24E0",
    "General Social Development" = "#24E0AB",
    "Employment" = "#2459E0"
  )) +
  
  labs(
    title = "Employment vs Welfare Focus in Beehive Releases \nby the Primary Minister for Social Development and Employment",
    subtitle = role_text,
    caption = "Source: Beehive NZ Government Data, Wikipedia",
    x = "Category",
    y = "Number of releases"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 8, face = "italic")
  )


plot3

ggsave("my_viz.png",plot3,width = 9, height = 10)

