# tidycovid Data.R
# Scraping google mobility data from pdfs
# Created by Aja Manu on 14/04/2020

# based on:
# https://joachim-gassen.github.io/2020/04/scrape-google-covid19-cmr-data/


#### Prelim---------------------------------------------------------------------

# install covid-19 package package
#remotes::install_github("joachim-gassen/tidycovid19")

# load library
library(tidyverse)
library(tidycovid19)
library(pdftools)
library(png)

#### Load PDF------------------------------------------------------------------

# base url
base_url <- "https://www.gstatic.com/covid19/mobility/"

#pdf_url <- paste0(base_url, Sys.Date(), "_AU_Mobility_Report_en.pdf")

pdf_url <- "https://www.gstatic.com/covid19/mobility/2020-04-05_AU_Mobility_Report_en.pdf"

pdf_convert(pdf_url, pages = 1, filenames = "google_cmr_au_p1.png", verbose = FALSE)

#### Get bitmap ----------------------------------------------------------------

bitmaps <- tidycovid19:::extract_line_graph_bitmaps(pdf_url, 1)
png_file <- tempfile("bitmap_", fileext = ".png")
writePNG(bitmaps[[1]][[1]], "bitmap.png")

#### Get data 

df <- tidycovid19:::parse_line_graph_bitmap(bitmaps[[1]][[1]])

ggplot(data = df, aes(x = date, y = measure)) + 
        geom_line(size = 2, color = "blue") + 
        geom_ribbon(
                aes(ymin = ifelse(measure > 0, 0, measure), ymax = 0), 
                fill = "blue", alpha = 0.2
        ) +
        theme_minimal() +
        scale_y_continuous(
                name="Retail & recreation", limits=c(-0.8, 0.8),
                breaks = c(-0.8, -0.4, 0, 0.4, 0.8)
        ) +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank()
        )

#### Load All Data--------------------------------------------------------------

# get merged 
merged_dta <- download_merged_data(cached = TRUE, silent = TRUE)

dta <- merged_dta %>% 
        filter(iso3c == "AUS", date >= "2020-02-23") %>%
        mutate(gov_interventions = (soc_dist + mov_rest)/
                       max(soc_dist + mov_rest, na.rm = TRUE),
               lockdown = lockdown == 1) %>%
        select(date, lockdown, gov_interventions, starts_with("gcmr_")) %>%
        pivot_longer(cols = c(3:9), names_to = "measure", values_to = "value") %>%
        na.omit()

ggplot(dta, aes(x = date, y = value, group = measure, color = measure)) + 
        theme_minimal() +
        annotate("rect", xmin = min(dta$date[dta$lockdown]), xmax = max(dta$date), 
                 ymin = -Inf, ymax = Inf, 
                 fill = "lightblue", color = NA, alpha = 0.2) +
        geom_line()   