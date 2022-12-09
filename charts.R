# This script analyses and presents the data collected visually

# Load packages and data
library(readr)
library(tidyverse)

world <- read_csv("Output/grandparents_world.csv")
ggplot(world, aes(x=year, y=number_grandparents))+geom_line()+ylim(c(0, 2e9))+theme_minimal()
ggplot(world, aes(x=year, y=share_grandparents))+geom_line()+theme_minimal()

# Get a few quotes for the text
country <- read_csv("Output/grandparents_by_country.csv")
country_22 <- country[country$year == '2022' & country$pop_un > 1000000, ]
head(country_22[order(country_22$share_grandparents), ])
tail(country_22[order(country_22$share_grandparents), ])
country_22_50 <- country[country$year %in% c(2022, 2050) & country$pop_un > 10000000, ]
country_22_50 <- country_22_50[order(country_22_50$year), ]
country_22_50$change <- ave(country_22_50$share_grandparents, country_22_50$iso3, FUN = function(x) x[2]-x[1])
country_22_50 <- country_22_50[!is.na(country_22_50$change), ]
country_22_50$change_prop <- country_22_50$change/country_22_50$share_grandparents
head(country_22_50[order(country_22_50$change), ])
tail(country_22_50[order(country_22_50$change), ])

# Quotes for text:
# "A typical grandparent in Senegal has TK grandchildren."
country[country$year == 2022 & country$iso3 == 'SEN', ]


# "A typical Mexican grandparent has only xx grandkids."

# " ."

