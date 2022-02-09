# Apple Mobility.R
# Look at Apple mobility data analysis
# Created by Aja Manu

#### Load Library---------------------------------------------------------------

# load library
library(tidyverse) # for working with tidydata
library(lubridate) # for working with dates

#### Load Data------------------------------------------------------------------

### Old

# previously manually downloaded from apple website then read in data
# data <- read_csv("./Data/applemobilitytrends-2020-05-20.csv")

### New

# get mobility data based on code adapted from below 
# https://kieranhealy.org/blog/archives/2020/05/23/get-apples-mobility-data/

get_apple_target <- function(cdn_url = "https://covid19-static.cdn-apple.com",
                             json_file = "covid19-mobility-data/current/v3/index.json") {
        tf <- tempfile(fileext = ".json")
        curl::curl_download(paste0(cdn_url, "/", json_file), tf)
        json_data <- jsonlite::fromJSON(tf)
        paste0(cdn_url, json_data$basePath, json_data$regions$`en-us`$csvPath)
}

get_apple_data <- function(url = get_apple_target(),
                           fname = "applemobilitytrends-",
                           date = stringr::str_extract(get_apple_target(), "\\d{4}-\\d{2}-\\d{2}"),
                           ext = "csv",
                           dest = "/Data",
                           save_file = c("n", "y")) {
        
        save_file <- match.arg(save_file)
        message("target: ", url)
        
        destination <- fs::path(here::here("/Data"),
                                paste0("applemobilitytrends-", date), ext = ext)
        
        tf <- tempfile(fileext = ext)
        curl::curl_download(url, tf)
        
        ## We don't save the file by default
        switch(save_file,
               y = fs::file_copy(tf, destination),
               n = NULL)
        
        readr::read_csv(tf)
}

data <- get_apple_data(save_file = "y")

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

### City Data

# plot data Australia
data1 %>% 
        filter(region %in% c("Australia", "Sydney", "Melbourne", "Brisbane", "Perth")) %>% 
        ggplot(aes(date, index, colour = region)) +
        geom_smooth(se = FALSE, span = 0.35) +
        geom_hline(yintercept = 100, colour = "red", linetype = "dashed") +
        geom_vline(xintercept = as.Date("2020-03-21"), colour = "red", linetype = "dashed") +
        geom_vline(xintercept = as.Date("2020-07-09"), colour = "red", linetype = "dashed") +
        scale_x_date(date_labels = "%b-%y") +
        scale_color_brewer(palette = "Pastel1") +
        facet_wrap(~transportation_type) +
        labs(title = paste("Mobility Data for Selected Australian Cities as at", 
                           format(max(data1$date), "%d %B")),
             subtitle = "Nation-wide social distancing recommendations commenced 21st March, \nVictoria additional social distancing commenced 9th July",
             x = NULL,
             y = "Index (100 = Baseline)",
             caption = "Source: https://www.apple.com/covid19/mobility") +
        theme_minimal() +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              axis.text.x = element_text(angle = 45),
              plot.title.position = "plot")

# plot data global
data1 %>% 
        filter(region %in% c("Sydney", "London", "Stockholm", "Tokyo", "Auckland",
                             "New York City")) %>% 
        ggplot(aes(date, index, colour = region)) +
        geom_smooth(se = FALSE, span = 0.4) +
        geom_hline(yintercept = 100, colour = "red", linetype = "dashed") +
        scale_x_date(date_labels = "%b-%y") +
        facet_wrap(~transportation_type) +
        labs(title = paste("Mobility Data for Selected Global Cities as at", 
                           format(max(data1$date), "%d %B")),
             x = NULL,
             y = "Index (100 = Baseline)",
             caption = "Source: https://www.apple.com/covid19/mobility") +
        theme_minimal() +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(angle = 45),
              plot.title.position = "plot")

### Country
data1 %>% 
        filter(region %in% c("Australia", "United Kingdom", "Germany", "Japan", "New Zealand",
                             "United States")) %>% 
        ggplot(aes(date, index, colour = region)) +
        geom_smooth(se = FALSE, span = 0.4) +
        geom_hline(yintercept = 100, colour = "red", linetype = "dashed") +
        scale_x_date(date_labels = "%b-%y") +
        facet_wrap(~transportation_type) +
        labs(title = paste("Mobility Data for Selected Global Cities as at", 
                           format(max(data1$date), "%d %B")),
             x = NULL,
             y = "Index (100 = Baseline)",
             caption = "Source: https://www.apple.com/covid19/mobility") +
        theme_minimal() +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(angle = 45),
              plot.title.position = "plot")
