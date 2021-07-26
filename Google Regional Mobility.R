# Google Regional Mobility.R
# Charts of regional mobility data from Google
# Created by Aja Manu on 26/07/2021


#### Prelim---------------------------------------------------------------------

# install mobility package
#remotes::install_github("datawookie/mobility")

#### Load Library---------------------------------------------------------------

library(mobility) # for Google mobility data
library(tidyverse) # for working with tidydata
library(tidyquant) # for working wiht timeseries data

#### Load Data------------------------------------------------------------------

# Load Australian data
mobility_au <- mobility("AU")

#### Filter Data----------------------------------------------------------------

# get NSW data
mobility_nsw <- mobility_au %>% 
        filter(region == "New South Wales")

#### Plot Data------------------------------------------------------------------

# chart data
mobility_nsw %>% 
        filter(sub_region %in% c("Sutherland Shire", "Fairfield City Council",
                                 "Waverley Council", "North Sydney Council")) %>% 
        unnest(data) %>% 
        filter(place %in% c("workplaces", "residential")) %>% 
        ggplot(aes(date, change, colour = place)) +
        #geom_smooth(se = FALSE, span = 0.2) +
        geom_ma(ma_fun = SMA, n = 7, linetype = 1, size = 1) +
        geom_hline(yintercept = 0, colour = "black") +
        facet_wrap(~ sub_region, nrow = 4, scales = "free_y") +
        labs(title = "Google Mobility Data for Selected NSW LGAs",
             subtitle = "The baseline day is the median value from the 5‑week period Jan 3 – Feb 6, 2020\n7-day average",
             x = "Date",
             y = "Change (%)",
             caption = "Source: https://www.google.com/covid19/mobility/") +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              legend.title = element_blank(),
              plot.title.position = "plot")
        