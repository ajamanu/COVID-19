# GDP vs COVID.R
# Chart for Why Australia
# Created by Aja Manu on 21/10/2020

#### Load Library---------------------------------------------------------------

library(tidyverse) # for working with tidy data
library(tidycovid19) # for getting COVID Data
library(OECD) # for getting OECD data
library(countrycode) # for getting country codes
library(janitor) # for leaning names
library(ggrepel) # for charting
library(gghighlight) # for charting

#### Load Data------------------------------------------------------------------

# load COVID-data
df <- download_merged_data(cached = TRUE, silent = TRUE)

# load economic 
# Get the data structure for Key Economic Indicators data set
dstruc <- get_data_structure("KEI")

# look at the data structure
str(dstruc, max.level = 1)

# look at data sets avaliable and choose the one we want
dstruc$SUBJECT

# get the Quarterly GDP PC data
gdp_data <- get_dataset("KEI", filter = list(c("NAEXKP01")))

# country codes
cc <- countrycode::codelist

#### Filter Data----------------------------------------------------------------

# countries we want
countries_of_interest <- tibble(country = c("China", "Japan", "South Korea" ,"United States", "India",
                                       "New Zealand", "Singapore", "United Kingdom",
                                       "Australia", "Vietnam", "Germany", "Ireland",
                                       "Netherlands", "Sweden", "Chile", "Brazil",
                                       "Belgium", "Colombia", "France", "Mexico",
                                       "Italy", "Argentina", "Spain", "Canada",
                                       "Greece"))

# join with data
countries_of_interest <- countries_of_interest %>% 
        left_join(cc %>% 
                          select(country.name.en, genc3c) %>% 
                          rename(country = country.name.en),
                  by = "country")

# get COVID data for countries we need
df_filtered <- df %>% 
        filter(iso3c %in% countries_of_interest$genc3c,
               date == max(date)) %>% 
        select(iso3c, country, date, ecdc_cases, ecdc_deaths, population)

# get gdp data for countries
gdp_data_filtered <- gdp_data %>% 
        clean_names(case = "lower_camel") %>% 
        filter(location %in% countries_of_interest$genc3c,
               measure == "GP",
               frequency == "Q",
               obsTime %in% c("2020-Q1", "2020-Q2")) %>% 
        select(location, obsTime, obsValue)

#### Chart Data-----------------------------------------------------------------

# cases/deaths per population
df_filtered <- df_filtered %>% 
        mutate(casesPerMil = ecdc_cases / population *1e6,
               deathsPerMil = ecdc_deaths / population * 1e6)

# get the decline in H1
gdp_data_filtered_h1 <- gdp_data_filtered %>% 
        group_by(location) %>% 
        mutate(index = ifelse(grepl("Q1", obsTime), 100 * (1 + obsValue/100), 0),
               index = ifelse(grepl("Q2", obsTime), lag(index) * (1 + obsValue/100), index),
               h1 = ifelse(grepl("Q2", obsTime), index - 100, NA)) %>% 
        drop_na(h1)

chart_data <- df_filtered %>% 
        left_join(gdp_data_filtered_h1, by = c("iso3c" = "location")) %>% 
        drop_na()

#### Plot Data------------------------------------------------------------------

# colours
# #2f6165

chart_data %>% 
        ggplot(aes(deathsPerMil, h1)) +
        geom_point(size = 3, colour = "#2f6165") +
        geom_text_repel(aes(label = country), point.padding = 0.1) +
        geom_smooth(se = FALSE, method = "lm", linetype = "dashed", colour = "lightgrey") +
        geom_point(data=chart_data %>% 
                           filter(country %in% c("Australia", "New Zealand")), 
                   aes(deathsPerMil, h1), color = "#00aaa1", size=3) +
        labs(x = "Cumulative deaths per million, as at 19 October",
             y = "Fall in GDP H1 2020 (%)",
             caption = "Source: ECDC, OECD, World Bank") +
        theme_minimal() +
        theme(panel.grid = element_blank())

chart_data %>% 
        ggplot(aes(casesPerMil, h1)) +
        geom_point(size = 3) +
        geom_text_repel(aes(label = country)) +
        #geom_smooth(se = FALSE, method = "lm", linetype = "dashed") +
        gghighlight(country %in% c("Australia", "New Zealand"))
