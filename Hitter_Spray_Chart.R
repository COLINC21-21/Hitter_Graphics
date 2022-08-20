library(tidyverse)
library(baseballr)
library(ggplot2)
Rizzo <- scrape_statcast_savant(start_date = "2022-04-05",
                                end_date = "2022-08-08",
                                playerid = 519203,
                                player_type = "batter")
str(Rizzo)

Rizzo %>%
  select(events,hc_x, hc_y) %>%
  head()

# get rid of non batted ball data for spray chart (don't need strike outs and walks in a spary chart)


Rizzo_bip <- Rizzo %>%
  filter(type == "X")
Rizzo_bip %>%
  select(events, hc_x, hc_y) %>%
  head()

## ggspraychart data = correa, fill_value = "events"
spray_chart <- function(...) {
  ggplot(...) + 
    geom_curve(x = 33, xend = 223, y = -100, yend = -100,
               curvature = -.65) +
    geom_segment(x = 128, xend = 33, y = -208, yend = -100) +
    geom_segment(x = 128, xend = 223, y = -208, yend = -100) +
    geom_curve(x = 83, xend = 173, y = -155, yend = -156,
               curvature = -.65, linetype = "dotted") +
    coord_fixed() + 
    scale_x_continuous(NULL, limits = c(25, 225)) + 
    scale_y_continuous(NULL, limits = c(-225, -25))
}

spray_chart(Rizzo_bip, aes(x = hc_x, y = -hc_y, color = events)) + 
  geom_point() + 
  labs(title = "Anthony Rizzo 2022",
       caption = "Colin Campbell\n Source:Baseballr")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(color = "brown"),
        plot.caption = element_text(color = "brown", face = 'bold'))