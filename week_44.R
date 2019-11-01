# week 44
# NYC Squirrel Census
# source: the NYC Squirrel Census - raw data at NY Data portal


library(leaflet)
library(tidyverse)
library(mapview)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

head(nyc_squirrels)
edit(nyc_squirrels)
names(nyc_squirrels)


# long format - considering columns with interactions w/ humans
squirrel_long <-
    nyc_squirrels %>%
    pivot_longer(cols = c("approaches", "indifferent", "runs_from"),
                 names_to = "human_interaction"
    )

squirrel_long <-
    squirrel_long %>%
    mutate(human_inter = ifelse(human_interaction == "approaches" & value == TRUE, "appro",
                                ifelse(human_interaction == "approaches" & value == FALSE, "nt_appr",
                                       ifelse(human_interaction == "indifferent" & value == TRUE, "indiff",
                                              ifelse(human_interaction == "indifferent" & value == FALSE, "nt_indiff",
                                                     ifelse(human_interaction == "runs_from" & value == TRUE, "runs",
                                                            ifelse(human_interaction == "runs_from" & value == FALSE, "nt_runs", "error")))))))


# squirrel long format, with only observations of interactions marked as TRUE
# (since it means that this observation was actually made)
squirrel_long_true <-
    squirrel_long %>%
    filter(value == TRUE)


#---------------------------
# visualizing with leaflet
# the basic leaflet to start: raw and long

# raw
squirrel_basic <-
    leaflet(nyc_squirrels) %>% addTiles()


# long
squirrel_basic_long <-
    leaflet(squirrel_long) %>% addTiles()


# long, filtered
squirrel_basic_long_filt <-
    leaflet(squirrel_long_true) %>% addTiles()


# Interaction w/ humans:
# approaches, indifferent, runs_from (and their opposite: "nt_")
# create palette of color for all levels of interaction

pal <- colorFactor("viridis", levels = c("appro", "nt_appr", "indiff", "nt_indiff",
                                       "runs", "nt_runs"))


# simple visualization with circle marks
squirrel_basic %>%
    addTiles() %>%
    addCircleMarkers(lng = ~long, lat = ~lat, radius = 1.2)


# not quite right, since it shows all the FALSE in the interactions
squirrel_basic_long %>%
    addTiles() %>%
    addCircleMarkers(lng = ~long, lat = ~lat, radius = 2, color = ~pal(human_inter)) %>%
    addLegend("bottomright", pal = pal, values = ~human_inter,
              title = "Human interactions",
              opacity = 1)


# probably most accurate so far: showing only the observation where there was a TRUE
# concerning a specific interaction
squirrel_plot <-
    squirrel_basic_long_filt %>%
    addTiles() %>%
    addCircleMarkers(lng = ~long, lat = ~lat, radius = 2, color = ~pal(human_inter)) %>%
    addLegend("bottomright", pal = pal, values = ~human_inter,
              title = "Human interactions",
              opacity = 1)

mapshot(squirrel_plot, file = "squirrel.png")


#-----------------------
# visualizing with ggplot 
# just to make sure that they are mostly indifferent with humans, as the map suggested

names(squirrel_long)

squirrel_long_true %>%
    group_by(human_inter) %>%
    summarize(sum = sum(value)) %>%
    ggplot()+
    geom_col(aes(x = human_inter, y = sum,  fill = human_inter), color = "black")+
    theme_classic()+
    scale_fill_viridis_d(breaks=c("appro", "indiff", "runs"), labels=c("Approaches",
                                                                       "Indifferent",
                                                                       "Runs away"))+
    theme(legend.position="bottom")+
    labs(fill = "Interaction with humans")+
    scale_y_continuous(breaks=seq(0,1500,by=200))+
    xlab("Interaction with human") + ylab("Amount of observations")

