# Tidy tuesday exercise: The Simpsons Guest Stars

library(tidyverse)
library(ggrepel)
library(cowplot)

simpsons <- read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv",
                       delim = "|")

head(simpsons)
edit(simpsons)
glimpse(simpsons)

# I'm interested in episodes where the guest star does some singing

guests_sing <-
    simpsons %>%
    filter(grepl("Sings", role))


# Visualization

guests <-
    guests_sing %>%
    ggplot(aes(x =  fct_infreq(guest_star)))+
    #geom_point(shape = 21, color = "black", size = 3.3)+
    geom_bar(stat = "count", color = "black", fill = "yellow", alpha = 0.7)+
    #scale_color_viridis_d()+
    #geom_text(stat = "count", aes(label = episode_title, y = ..count..))+
    coord_flip()+
    xlab("Guest Star")+
    theme_dark()+
    theme(legend.position="none")+
    ggtitle("Guest stars who sings")

michaeld <-
    guests_sing %>%
    filter(guest_star == "Michael Dees") %>%
    ggplot(aes(x = order(season), y = number))+
    geom_point(color = "black",fill = "blue", shape = 21, size = 3)+
    geom_text_repel(aes(label = episode_title))+
    theme_classic()+
    ylab("Number of episode")+
    xlab("Season")+
    ggtitle("Singing participation of Michael Dees")


ggdraw(guests + theme_half_open(13)) +
    draw_plot(michaeld, .45, .45, .5, .5) +
    draw_plot_label(
        c("A", "B"),
        c(0, 0.45),
        c(1, 0.95),
        size = 10
    )
