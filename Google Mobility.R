# Google Mobility.R
# Analysis of Google mobility reports
# Created by Aja Manu on 02/07/2020

#### Load Library---------------------------------------------------------------

library(tidyverse)
library(tidycovid19)
library(zoo)
library(lubridate)
library(ggrepel)
library(gghighlight)

#### Get Data-------------------------------------------------------------------

# download data
df <- download_merged_data(cached = TRUE, silent = TRUE)


#### Plot Charts----------------------------------------------------------------
df %>% 
        filter(income == "High income") %>% 
        select(iso3c, date, gcmr_retail_recreation) %>% 
        drop_na() %>%
        group_by(iso3c) %>% 
        mutate(moving_average = rollmean(gcmr_retail_recreation, k = 7, align = "right",
                                         fill = NA)) %>% 
        filter(date == max(date)) %>% 
        ungroup() %>% 
        mutate(iso3c = fct_reorder(iso3c, -moving_average)) %>% 
        ggplot(aes(iso3c, -moving_average, fill = iso3c)) +
        geom_col(show.legend = FALSE) + 
        coord_flip()

