# Apple Mobility.R
# Look at Apple mobility data analysis
# Created by Aja Manu

#### Load Library---------------------------------------------------------------

# load library
library(tidyverse) # for working with tidydata
library(lubridate) # for working with dates

#### Load Data------------------------------------------------------------------

data <- read_csv("./Data/applemobilitytrends-2020-05-20.csv")

#### Clean Data-----------------------------------------------------------------

# pivot data frame
data1 <- data %>% 
        select(-c(geo_type, alternative_name:country)) %>% 
        pivot_longer(-c(region, transportation_type), names_to = "date", 
                     values_to = "index")

# fix data
data1 <- data1 %>% 
        mutate(date = ymd(date))

#### Plot Chart-----------------------------------------------------------------

# plot data Australia
data1 %>% 
        filter(region %in% c("Sydney", "Melbourne", "Brisbane", "Perth")) %>% 
        ggplot(aes(date, index, colour = region)) +
        geom_smooth(se = FALSE) +
        geom_hline(yintercept = 100, colour = "red", linetype = "dashed") +
        geom_vline(xintercept = as.Date("2020-03-21"), colour = "red", linetype = "dashed") +
        facet_wrap(~transportation_type) +
        labs(title = paste("Mobility Data for Selected Australian Cities as at", 
                           format(max(data1$date), "%d %B")),
             subtitle = "Nation-wide social distancing recommendations commenced 21st March",
             x = NULL,
             y = "Index (100 = Baseline)",
             caption = "Source: https://www.apple.com/covid19/mobility") +
        theme_minimal() +
        theme(legend.title = element_blank())

# plot data global
data1 %>% 
        filter(region %in% c("Sydney", "London", "Stockholm", "Tokyo", "Auckland",
                             "New York City")) %>% 
        ggplot(aes(date, index, colour = region)) +
        geom_smooth(se = FALSE) +
        geom_hline(yintercept = 100, colour = "red", linetype = "dashed") +
        facet_wrap(~transportation_type) +
        labs(title = paste("Mobility Data for Selected Global Cities as at", 
                           format(max(data1$date), "%d %B")),
             x = NULL,
             y = "Index (100 = Baseline)",
             caption = "Source: https://www.apple.com/covid19/mobility") +
        theme_minimal() +
        theme(legend.title = element_blank())

