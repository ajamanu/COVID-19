# OpenTable.R
# Look at the state of the restaurant industry
# Created by Aja Manu on 22/06/2020

#### Load Library---------------------------------------------------------------

# load library
library(tidyverse) # for working with tidydata
library(lubridate) # for working with dates
library(janitor) # for cleaning names

#### Load Data------------------------------------------------------------------

# Data avaliable from:
# https://www.opentable.com/state-of-industry

data <- read_csv("./Data/OpenTable/YoY_Seated_Diner_Data.csv")

#### Clean Data-----------------------------------------------------------------

# pivot data frame
data1 <- data %>% 
        pivot_longer(-c(Type, Name), names_to = "date", 
                     values_to = "pctCh")

# fix dates
data1 <- data1 %>% 
        mutate(date = paste0(date, "/2020"),
               date = mdy(date))

# clean names
data1 <- clean_names(data1, case = "lower_camel")

#### Plot Chart-----------------------------------------------------------------

# plot country data
data1 %>% 
        filter(type == "country", 
        !(name %in% c("Global", "Ireland", "Mexico"))) %>% 
        ggplot(aes(date, pctCh, colour = name)) +
        geom_line(size  = 1) +
        #geom_smooth(se = FALSE) +
        geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
        labs(title = paste("State of Restaurant Industry as at", 
                           format(max(data1$date), "%d %B")),
             subtitle = "Year-on-year percentage change in diners dining at restaurants",
             x = NULL,
             y = "Per cent change (%)",
             caption = "Source: https://www.opentable.com/state-of-industry") +
        theme_minimal() +
        theme(legend.title = element_blank())