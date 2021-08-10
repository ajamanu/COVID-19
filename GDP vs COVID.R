# GDP vs COVID.R
# Chart for Why Australia
# Created by Aja Manu on 21/10/2020

#### Load Library---------------------------------------------------------------

# load fonts
loadfonts(device="win")

library(tidyverse) # for working with tidy data
library(tidycovid19) # for getting COVID Data
library(OECD) # for getting OECD data
library(countrycode) # for getting country codes
library(janitor) # for leaning names
library(ggrepel) # for charting
library(gghighlight) # for charting
library(writexl) # for writing out excel files
library(extrafont) # for plotting

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
                                        "Sweden", "Brazil", "Belgium", "France", 
                                       "Italy", "Argentina", "Spain", "Canada"))

# join with data
countries_of_interest <- countries_of_interest %>% 
        left_join(cc %>% 
                          select(country.name.en, genc3c) %>% 
                          rename(country = country.name.en),
                  by = "country")

# get COVID data for countries we need
df_filtered <- df %>% 
        filter(iso3c %in% countries_of_interest$genc3c,
               date == "2021-03-31") %>% # update the date to what you want
        select(iso3c, country, date, ecdc_cases, ecdc_deaths, population)

# get gdp data for countries
gdp_data_filtered <- gdp_data %>% 
        clean_names(case = "lower_camel") %>% 
        filter(location %in% countries_of_interest$genc3c,
               measure == "GP",
               frequency == "Q",
               obsTime %in% c("2020-Q1", "2020-Q2", "2020-Q3", "2020-Q4", 
                              "2021-Q1")) %>% 
        select(location, obsTime, obsValue)

#### Chart Data-----------------------------------------------------------------

# cases/deaths per population
df_filtered <- df_filtered %>% 
        mutate(casesPerMil = ecdc_cases / population *1e6,
               deathsPerMil = ecdc_deaths / population * 1e6)

# get the cumulative growth
gdp_data_filtered_cum <- gdp_data_filtered %>%
        group_by(location) %>%
        mutate(index = (1+obsValue/100) %>% accumulate( ~.x*.y)) %>% 
        ungroup() %>% 
        mutate(index = index*100)

# join data 
chart_data <- df_filtered %>% 
        left_join(gdp_data_filtered_cum, by = c("iso3c" = "location")) %>% 
        drop_na()

# get total gdp growth
chart_data <- chart_data %>%
        mutate(totalGrowth = index - 100) %>%
        group_by(country) %>%
        slice(n())%>%
        ungroup()

#### Plot Data------------------------------------------------------------------

# CPEC colours
# #2f6165, #dcf3f2
# #d9d9d9 For axis

# plot chart
chart_data %>% 
        filter(!(country %in% c("Ireland", "Brazil", "India"))) %>% 
        ggplot(aes(deathsPerMil, totalGrowth)) +
        geom_point(size = 5, colour = "#dcf3f2") +
        geom_text_repel(aes(label = country), point.padding = 0.22) +
        geom_smooth(se = FALSE, method = "lm", linetype = "dashed", colour = "lightgrey") +
        geom_point(data=chart_data %>% 
                           filter(country %in% c("Australia", "New Zealand")), 
                   aes(deathsPerMil, totalGrowth), color = "#00aaa1", size=5) +
        geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
        labs(x = "Cumulative deaths per million, as at 31 March 2021",
             y = "Growth in GDP From Start of 2020 to Q1 2021 (%)") +
            # caption = "Source: ECDC, OECD, World Bank") +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              axis.line = element_line(colour = "#d9d9d9"),
              text = element_text(family = "Segoe UI"))

chart_data %>% 
        ggplot(aes(casesPerMil, totalGrowth)) +
        geom_point(size = 3) +
        geom_text_repel(aes(label = country)) +
        #geom_smooth(se = FALSE, method = "lm", linetype = "dashed") +
        gghighlight(country %in% c("Australia", "New Zealand"))

#### Export Data----------------------------------------------------------------

write_xlsx(chart_data, paste0("Output/", Sys.Date() ," GDP vs COVID-19 Final.xlsx"))

write_xlsx(gdp_data_filtered, "OECD GDP Data.xlsx")

write_xlsx(df_filtered, "COVID Data.xlsx")