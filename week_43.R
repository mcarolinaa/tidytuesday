# get the data
horror <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")


library(tidyverse)
library(lubridate)
library(ggthemes)

# checking
head(horror)
glimpse(horror)
names(horror)

# general summary
horror %>%
    summary()


# create new columns
horror <-
    horror %>%
    mutate(date = dmy(release_date),
           rev_cls = ifelse(review_rating >= 5.1, "above_av", "below_av")) 

# create normalized review variable
horror$rev_nmz <- scale(horror$review_rating)
horror <- horror[order(horror$rev_nmz),]


# total of release movies per country:
# finding the top 10 countries that most produce horror movies
top10_count <-
    horror %>%
    group_by(release_country, title) %>%
    summarise(num_countries = n_distinct(release_country)) %>%
    group_by(release_country) %>%
    summarise(sum_country = sum(num_countries)) %>%
    arrange(desc(sum_country)) %>%
    slice(1:10) 
    
    
# working w/ the top10 releasing countries subset
subs_ct <- subset(horror, horror$release_country == top10_count$release_country)


# plotting the reviews x genres from the top10 releasing countries 
subs_ct %>%
    filter(rev_cls != "NA") %>%
    ggplot(aes(x = fct_reorder(genres, rev_nmz), y = rev_nmz, label = release_country))+
    geom_point(aes(fill = rev_cls), stat = "identity", shape = 21, size = 7.5,  alpha = 0.6)+
    geom_text(color="grey5", size=2.1) +
    ylim(-2.5, 2.5) +
    coord_flip()+
    theme_dark()+
    scale_fill_manual(values=c("green", "red"))+
    labs(fill = "Review ratings")+
    xlab("Movies genres") + ylab("Normalized review ratings")+
    ggtitle("Horror movies genres and review ratings from top10 releasing countries",
            subtitle = "Declared genres like Horror/Horror Thriller have more titles
            and broader ratings")+
    theme(text=element_text(size=13))+
    theme(plot.background = element_rect(fill = "grey"))+
    theme(legend.position = c(0.95, 0.15),
        legend.justification = c("right", "top"))
