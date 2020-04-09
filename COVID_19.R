# COVID-19.R
# Get COVID-19 number of cases
# Created by Aja Manu on 25/03/2019 
# updatd on 09/04/2020

#### Load Libraries-------------------------------------------------------------

# load library
library(tidyverse) # for working with tidy data
library(janitor) # for cleaning names
library(readxl) # reading excel files
library(httr) # connecting to websites
library(ggrepel) # for plotting
library(scales) # for plotting
library(zoo) # for rolling mean

#### Load Data------------------------------------------------------------------

# code to load data adapted from below 
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

# Note have adapted the code due to timezone differences

# create the URL where the dataset is stored with automatic updates every day
#url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
#             format(.POSIXct(Sys.time(),tz="GMT"), "%Y-%m-%d"), ".xlsx", sep = "")

#url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-24.xlsx"

#download the dataset from the website to a local temporary file
#GET(url, authenticate(":", ":", type="ntlm"), 
#    write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into “R”
#data <- read_excel(tf)

data <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na = "")

#### Clean Data-----------------------------------------------------------------

# clean column names
data <- clean_names(data, case = "lower_camel")

# fix date
data <- data %>% 
        mutate(dateRep = as.Date(dateRep, "%d/%m/%Y"))

#### Create New Variables-------------------------------------------------------

# Calculate cumulative cases
# https://kieranhealy.org/blog/archives/2020/03/21/covid-19-tracking/
cov_curve <- data %>%
        select(dateRep, countriesAndTerritories, geoId, cases, deaths) %>%
        group_by(geoId) %>%
        arrange(dateRep) %>%
        mutate(cumCases = cumsum(cases), 
               cumDeaths = cumsum(deaths)) %>%
        filter(cumCases > 100) %>%
        mutate(daysElapsed = dateRep - min(dateRep),
               end_label = ifelse(dateRep == max(dateRep), countriesAndTerritories, NA))

cov_curve1 <- data %>%
        select(dateRep, countriesAndTerritories, geoId, cases, deaths) %>%
        group_by(geoId) %>%
        arrange(dateRep) %>%
        mutate(cumCases = cumsum(cases), 
               cumDeaths = cumsum(deaths)) %>%
        filter(cumDeaths > 10) %>%
        mutate(daysElapsed = dateRep - min(dateRep),
               end_label = ifelse(dateRep == max(dateRep), countriesAndTerritories, NA))

# calculate rolling average 
cov_curve_a <- data %>%
    select(dateRep, countriesAndTerritories, geoId, cases, deaths) %>%
    group_by(geoId) %>%
    arrange(dateRep) %>%
    mutate(cumCases = cumsum(cases), 
           cumDeaths = cumsum(deaths),
           avgCases = rollmean(cases, 7, align = "right", fill = NA),
           avdDeaths = rollmean(deaths, 7, align = "right", fill = NA)) %>%
    filter(avgCases > 30) %>%
    mutate(daysElapsed = dateRep - min(dateRep),
           end_label = ifelse(dateRep == max(dateRep), countriesAndTerritories, NA))

cov_curve_a1 <- data %>%
    select(dateRep, countriesAndTerritories, geoId, cases, deaths) %>%
    group_by(geoId) %>%
    arrange(dateRep) %>%
    mutate(cumCases = cumsum(cases), 
           cumDeaths = cumsum(deaths),
           avgCases = rollmean(cases, 7, align = "right", fill = NA),
           avgDeaths = rollmean(deaths, 7, align = "right", fill = NA)) %>%
    filter(avgDeaths > 3) %>%
    mutate(daysElapsed = dateRep - min(dateRep),
           end_label = ifelse(dateRep == max(dateRep), countriesAndTerritories, NA))

#### Get Pop Data---------------------------------------------------------------

# get population data
pop <- data %>%
    select(countriesAndTerritories, countryterritoryCode, popData2018) %>% 
    distinct() %>% 
    drop_na()

#### Plot Data------------------------------------------------------------------

# Countries to focus on
focus_cn <- c("CN", "UK", "US", "IR", "JP",
              "KR", "IT", "FR", "ES", "AU", "NZ")

### Cumulative Basis

# Plot curves adapted from
# https://kieranhealy.org/blog/archives/2020/03/21/covid-19-tracking/
cov_curve %>%
        filter(geoId %in% focus_cn) %>% ## focus on ust a few countries, defined above
        mutate(end_label = recode(end_label, `United_States_of_America` = "US",
                                  `Iran, Islamic Republic of` = "Iran", 
                                  `South_Korea` = "South Korea", 
                                  `United_Kingdom` = "UK",
                                  New_Zealand = "NZ")) %>% 
        ggplot(mapping = aes(x = daysElapsed, y = cumCases, 
                             color = countriesAndTerritories, label = end_label, 
                             group = countriesAndTerritories)) + 
        geom_line(size = 0.8) + 
        geom_text_repel(nudge_x = 1.1,
                        nudge_y = 0.1, 
                        segment.color = NA) + 
        guides(color = FALSE) +
        scale_y_continuous(labels = scales::comma_format(accuracy = 1), 
                           breaks = trans_breaks("log2", function(x) 2^x),
                           trans = "log2") +
        labs(x = "Days Since 100th Confirmed Case", 
             y = "Cumulative Number of Cases (log scale)", 
             title = "Cumulative Cases from COVID-19, Selected Countries", 
             subtitle = paste("Data as of", format(max(cov_curve$dateRep), "%A, %B %e, %Y")), 
             caption = "Data: ECDC") +
        theme_minimal() +
        coord_cartesian(xlim = c(0, 50))

# Plot curves adapted from
# https://kieranhealy.org/blog/archives/2020/03/21/covid-19-tracking/
cov_curve1 %>%
        filter(geoId %in% focus_cn) %>% ## focus on ust a few countries, defined above
        mutate(end_label = recode(end_label, `United_States_of_America` = "US",
                                  `Iran, Islamic Republic of` = "Iran", 
                                  `South_Korea` = "South Korea", 
                                  `United_Kingdom` = "UK",
                                  New_Zealand = "NZ")) %>% 
        ggplot(mapping = aes(x = daysElapsed, y = cumDeaths, 
                             color = countriesAndTerritories, label = end_label, 
                             group = countriesAndTerritories)) + 
        geom_line(size = 0.8) + 
        geom_text_repel(nudge_x = 1.1,
                        nudge_y = 0.1, 
                        segment.color = NA) + 
        guides(color = FALSE) +
        scale_y_continuous(labels = scales::comma_format(accuracy = 1), 
                           breaks = trans_breaks("log2", function(x) 2^x),
                           trans = "log2") +
        labs(x = "Days Since 10th Confirmed Death", 
             y = "Cumulative Number of Deaths (log scale)", 
             title = "Cumulative Deaths from COVID-19, Selected Countries", 
             subtitle = paste("Data as of", format(max(cov_curve1$dateRep), "%A, %B %e, %Y")), 
             caption = "Data: ECDC") +
        theme_minimal() +
        coord_cartesian(xlim = c(0, 45))

### Average basis

# plot cases
cov_curve_a %>%
    filter(geoId %in% focus_cn) %>% ## focus on ust a few countries, defined above
    mutate(end_label = recode(end_label, `United_States_of_America` = "US",
                              `Iran, Islamic Republic of` = "Iran", 
                              `South_Korea` = "South Korea", 
                              `United_Kingdom` = "UK",
                              New_Zealand = "NZ")) %>% 
    ggplot(mapping = aes(x = daysElapsed, y = avgCases, 
                         color = countriesAndTerritories, label = end_label, 
                         group = countriesAndTerritories)) + 
    geom_line(size = 0.8) + 
    geom_text_repel(nudge_x = 1.1,
                    nudge_y = 0.1, 
                    segment.color = NA) + 
    guides(color = FALSE) +
    scale_y_continuous(labels = scales::comma_format(accuracy = 1), 
                       breaks = trans_breaks("log2", function(x) 2^x),
                       trans = "log2") +
    geom_text_repel(nudge_x = 1.1,
                    nudge_y = 0.1, 
                    segment.color = NA) + 
    guides(color = FALSE) +
    scale_y_continuous(labels = scales::comma_format(accuracy = 1), 
                       breaks = trans_breaks("log2", function(x) 2^x),
                       trans = "log2") +
    labs(x = "Days Since 30 (7- day rolling average) Confirmed Cases", 
         y = "Average Number of Cases (log scale)", 
         title = "Average Cases from COVID-19, Selected Countries", 
         subtitle = paste("Data as of", format(max(cov_curve$dateRep), "%A, %B %e, %Y")), 
         caption = "Data: ECDC") +
    theme_minimal()

# plot deaths
cov_curve_a1 %>%
    filter(geoId %in% focus_cn) %>% ## focus on ust a few countries, defined above
    mutate(end_label = recode(end_label, `United_States_of_America` = "US",
                              `Iran, Islamic Republic of` = "Iran", 
                              `South_Korea` = "South Korea", 
                              `United_Kingdom` = "UK",
                              New_Zealand = "NZ")) %>% 
    ggplot(mapping = aes(x = daysElapsed, y = avgDeaths, 
                         color = countriesAndTerritories, label = end_label, 
                         group = countriesAndTerritories)) + 
    geom_line(size = 0.8) + 
    geom_text_repel(nudge_x = 1.1,
                    nudge_y = 0.1, 
                    segment.color = NA) + 
    guides(color = FALSE) +
    scale_y_continuous(labels = scales::comma_format(accuracy = 1), 
                       breaks = trans_breaks("log2", function(x) 2^x),
                       trans = "log2") +
    geom_text_repel(nudge_x = 1.1,
                    nudge_y = 0.1, 
                    segment.color = NA) + 
    guides(color = FALSE) +
    scale_y_continuous(labels = scales::comma_format(accuracy = 1), 
                       breaks = trans_breaks("log2", function(x) 2^x),
                       trans = "log2") +
    labs(x = "Days Since 3 (7- day rolling average) Deaths", 
         y = "Average Number of Deaths (log scale)", 
         title = "Average Deaths from COVID-19, Selected Countries", 
         subtitle = paste("Data as of", format(max(cov_curve$dateRep), "%A, %B %e, %Y")), 
         caption = "Data: ECDC") +
    theme_minimal()

### Per-capita basis

# percapita basis 
per_cap <- cov_curve %>% 
    left_join(., pop) %>% 
    mutate(cumCasesPerCap = cumCases/popData2018*100000) %>% 
    filter(geoId %in% focus_cn,
           dateRep == max(dateRep)) %>% ## focus on ust a few countries, defined above
    mutate(end_label = recode(end_label, `United_States_of_America` = "US",
                              `Iran, Islamic Republic of` = "Iran", 
                              `South_Korea` = "South Korea", 
                              `United_Kingdom` = "UK",
                              New_Zealand = "NZ"))