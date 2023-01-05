# 1. Plots ------

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

# Average age of a grandparent:
res <- data.frame()
for(i in setdiff(dir('Output'), c("grandparents_age.pdf", "grandparents_by_country_age.csv", "grandparents_by_country.csv", "grandparents_over_time.pdf", "grandparents_vs_65+.pdf",  "grandparents_world.csv", "grandparents.pdf"))){
  temp <- read_csv(paste0('Output/',i))
  for(j in c(2022)){
  temp <- temp[temp$year == '2022', ]
  res <- rbind(res, c(temp$country[1], weighted.mean(temp$age, temp$gp)))
  }
}
colnames(res) <- c('country', 'mean_grandparent_age')

tail(res[order(res$mean_grandparent_age), ])
head(res[order(res$mean_grandparent_age), ])

# 2. Average number of grandchildren -------

# "A typical Mexican grandparent has only xx grandkids."

# Use DemoKin to get average number of grandchildren for a 'typical' grandparent. 
# Let's take 'typical' to mean a grandparent aged 'x', where 'x' is the mean age
# of all grandparents in a population, as estimated by Sondre from the simulations. 

# We use DemoKin to avoid having to re-read the simulations and because it's a cleaner
# mathematical implementation: https://github.com/IvanWilli/DemoKin.

# devtools::install_github("IvanWilli/DemoKin", build_vignettes = TRUE)
library(DemoKin)
library(countrycode)

# Download data from UNWPP for kinship models
get_UNWPP_inputs_DemoKin <- function(countries, my_startyr, my_endyr, variant = "Median"){
  
  
  print("Getting API ready...")
  # Get data from UN using API
  
  base_url <- 'https://population.un.org/dataportalapi/api/v1'
  
  # First, identify which indicator codes we want to use
  
  target <- paste0(base_url,'/indicators/?format=csv')
  codes <- read.csv(target, sep='|', skip=1) 
  
  qx_code <- codes$Id[codes$ShortName == "qx1"]
  asfr_code <- codes$Id[codes$ShortName == "ASFR1"]
  pop_code <- codes$Id[codes$ShortName == "PopByAge1AndSex"]
  
  # Get location codes
  
  target <- paste0(base_url, '/locations?sort=id&format=csv')
  df_locations <- read.csv(target, sep='|', skip=1)
  
  # find the codes for countries
  
  my_location <- 
    df_locations %>% 
    filter( Name %in% countries) %>% 
    pull(Id) %>% 
    paste(collapse = ",")
  
  # Get px values
  
  print(paste0("Getting mortality data for ", paste(countries, collapse = ", ")))
  
  my_indicator <- qx_code
  my_location  <- my_location
  
  target <- paste0(base_url,
                   '/data/indicators/',my_indicator,
                   '/locations/',my_location,
                   '/start/',my_startyr,
                   '/end/',my_endyr,
                   '/?format=csv')
  
  px <- 
    read.csv(target, sep='|', skip=1) %>% 
    filter(Variant %in% variant) %>% 
    filter(Sex == "Female") %>% 
    mutate(px = 1- Value) %>% 
    select(Location, Time = TimeLabel, age = AgeStart, px)
  
  # ASFR
  
  print(paste0("Getting fertility data for ", paste(countries, collapse = ", ")))
  
  my_indicator <- asfr_code
  
  target <- paste0(base_url,
                   '/data/indicators/',my_indicator,
                   '/locations/',my_location,
                   '/start/',my_startyr,
                   '/end/',my_endyr,
                   '/?format=csv')
  
  asfr <- 
    read.csv(target, sep='|', skip=1) %>% 
    filter(Variant %in% variant) %>% 
    select(Location, Time = TimeLabel, age = AgeStart, ASFR = Value)
  
  data <- 
    px %>% 
    left_join(asfr, by = c("Location", "Time", "age")) %>% 
    mutate(ASFR = replace(ASFR,is.na(ASFR),0)) 
  
  data
}

# function to get average number of grandchildren for a given country for 2022
get_num_granchildren_in_2022 <- function(country, res){
  
  # Year range
  
  my_startyr   <- 1950
  my_endyr     <- 2022
  
  data <- get_UNWPP_inputs_DemoKin(
    countries = country
    , my_startyr = my_startyr
    , my_endyr = my_endyr
  )
  
  U <-
    data %>%
    select(Time, age, px) %>%
    pivot_wider(names_from = Time, values_from = px) %>%
    select(-age) %>% 
    as.matrix()
  
  f <- data %>%
    select(Time, age, ASFR) %>%
    mutate(ASFR = ASFR/1000) %>% 
    pivot_wider(names_from = Time, values_from = ASFR) %>%
    select(-age) %>% 
    as.matrix()
  
  # Run kinship models only for granddaughters
  k <- kin(U, f, time_invariant = FALSE, output_kin = c("gd"), output_period = 2022)
  
  # Keep grandparents aged x
  gp_age <- floor(as.numeric(res$mean_grandparent_age[res$country %in% country]))

  df <- 
    k$kin_summary %>% 
    filter(age_focal %in% gp_age) %>% 
    mutate(
      # Kinship models are female and matrilineal, so we approximate both-sex 
      # grandchildren by multiplying by 4
      num_gc = round(count_living*4, 1)
      , age_diff = round(age_focal - mean_age, 0)
      , country = country
      ) %>% 
    select(country, `Number of grandchildren` = num_gc, `Age difference` = age_diff)
  
  df
  
    # paste0("An average grandparent in ", country, " has ", df$num_gc, " grandchildren.")
    # paste0("Grandparents are about ", df$age_diff, " years older than their grandkids.")
  
}

# Try it out:

get_num_granchildren_in_2022(country = "Senegal", res = res)

get_num_granchildren_in_2022(country = "Mexico", res = res)

get_num_granchildren_in_2022(country = "Sweden", res = res)
