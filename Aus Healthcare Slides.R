# Aus Healthcare Slides.R
# Slides for JH on Australian Healthcare
# Created by Aja Manu on 17/04/2020

#### Load Library---------------------------------------------------------------

# load library
library(tidyverse)
library(ggrepel)
library(ggforce)
library(scales)


#### load Data------------------------------------------------------------------

data1 <- read_csv("U:/Data/Mics/COVID19/OECD/Healthcare Exp vs Life Exp.csv")


#### Plot-----------------------------------------------------------------------

### Life Exp vs Healthcare Exp % of GDP
data1a <- data1
data1a$label <- data1a$country # get point names
points_to_label <- c("Australia", "United Kingdom", "United States", "New Zealand",
                     "Japan", "Poland", "France", "Korea")
data1a$label[!(data1a$label %in% points_to_label)] <- ""

data1a %>% 
        ggplot(aes(healthGdp/100, lifeExp, label = label)) +
        geom_point(color = ifelse(data1a$label == "Australia", "#2f6165", "grey50"), size = 5) +
        geom_smooth(se = FALSE, method = lm, colour = "#083555", size = 2, linetype = "dashed") +
        geom_text_repel(hjust = 0, size = 7) +
        labs(title = "Life Expectancy vs Healthcare Expenditure",
             x = "Healthcare Expenditure % of GDP",
             y = "Life Expectancy at Birth",
             caption = "Source: OECD Health Statistics 2019") +
        scale_x_continuous(labels = scales::percent) +
        theme_minimal() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              plot.title = element_text(size = 30, face = "bold"))