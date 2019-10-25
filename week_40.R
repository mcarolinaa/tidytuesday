# Pizza Party! dataset
# Source: DataFiniti

library(tidyverse)

# Get data
pizza_datafiniti <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_datafiniti.csv")

# Working w/ 1 dataset
head(pizza_datafiniti)
glimpse(pizza_datafiniti)

# I'll make 2 plots:

#-------
# Plot 1 - spatial analysis
library(leaflet)
library(mapview)

# just a basic map, no marks
pizza_map <-
    leaflet(pizza_datafiniti) %>% addTiles()

# adding some marks: circles to the locations and clusters
pizza_map <-
    pizza_map %>%
    addTiles() %>%
    addCircles(lng = ~longitude, lat = ~latitude, weight = 8,
               radius = 100, stroke = TRUE)%>%
    addMarkers(clusterOptions = markerClusterOptions())

mapshot(pizza_map, file = "pizza_1.png")

#-------
# Plot 2
library(cowplot)

# summarizing number of places per state
per_states <-
    pizza_datafiniti %>%
    group_by(province) %>%
    summarise(n1 = n_distinct(name)) %>%
    group_by(province) %>%
    summarise(total_per_prov = sum(n1)) %>%
    #top_n(n = 10) %>%
    edit()
    
 # checking top10 states:
    pizza_datafiniti %>%
    group_by(province) %>%
    summarise(n1 = n_distinct(name)) %>%
    group_by(province) %>%
    summarise(total_per_prov = sum(n1)) %>%
    top_n(n = 10) %>%
    edit()

ny_df <- data.frame(x1 = "NY", y1 = 395, x2 = "MI", y2 = 350)

pizza_plot <-
    ggplot(data = per_states, aes(x = fct_reorder(province, total_per_prov), y = total_per_prov))+
    geom_point()+
    geom_col(color = "black", alpha = 0.7, fill = "tomato2")+
    coord_flip()+
    labs(title = "Pizza places per state",
        subtitle = "Amount of pizza places/restaurants with pizza per state in USA",
        caption = "Source:")+
    xlab("USA State") + ylab("Amount of Pizza places")+
    scale_y_continuous(breaks=seq(0,400,by=50))+
    theme_minimal_hgrid()+
    theme(plot.background = element_rect(fill = "cornsilk", color = NA))+
    geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2),
               data = ny_df, color = "seagreen4", size = 2, arrow = arrow(length = unit(0.03, "npc")))+
    annotate("text", x = "GA", y = 350, label = "New York is \nthe winning state!", color = "seagreen4", size = 6, hjust = .5)

