
# Answer question 'how many grandparents there are in the world'
# I'll try answering this with simulation now using the SOCSIM outputs from the paper:
# Alburez‐Gutierrez, D., Mason, C., and Zagheni, E. (2021). The “Sandwich Generation” Revisited: Global Demographic Drivers of Care Time Demands. Population and Development Review 47(4):997–1023. doi:10.1111/padr.12436.

# Replication data is stored in the Harvard Dataverse
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SSZL6U

# 0. Preamble ------

rm(list=ls())
library(tidyverse)
library(httr)
library(countrycode)

# countries <- c("Germany", "Guatemala", "China", "USA")
countries <- c("all")
# How many simulations per country? max 5
# Don't touch this
num_sims <- 1

years <- seq(1990, 2040, 5)

# Don't touch this
get_un_pop_from_api <- F

# To convert months to yeares in SOCSIM
# 20200414 This should work for new estimates up to 2200
FinalSimYear <-  2200
endmo <-  5400

# useful functions

# a function to convert socsim months to calendar years
asYr2 <- function(x, FinalSimYear, endmo) {
  return(trunc(FinalSimYear - (endmo - x)/12) +1)
}

# Get data in dataveres
list_data <- function(){
  print("Getting API ready...")
  
  # 1. Get links to download simulation data from Harvard Dataverse 
  # https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SSZL6U
  
  # Get list of files from Datavsers
  api_call <- "https://dataverse.harvard.edu/api/datasets/:persistentId?persistentId=doi:10.7910/DVN/SSZL6U"
  
  res <- GET(api_call)
  strikes <- jsonlite::fromJSON(content(res, 'text'), simplifyVector = FALSE)
  
  data_list <- strikes$data$latestVersion$files
  
  data_df <- 
    lapply(data_list, function(x) data.frame(x)) %>% 
    bind_rows() %>% 
    mutate(
      country = str_extract(label, "[A-z]+")
      , country = gsub("_", "", country)
      , seed = str_extract(label, "[0-9]+")
    ) %>% 
    select(country, seed , label, id = dataFile.id)
  
  return(data_df)
}

find_grandparents <- function(countries, year, data_df, export = T){
  
  # Avoid unnecessary calculations by NOT running again analysis for countries
  # that alrady have a saved csv file
  if(export){
    f <- list.files("Output", pattern = "gp_")
    f <- gsub("gp_|.csv", "", f)
    f <- gsub("_[0-9]+", "", f)
    f <- gsub("_", "", f)
    avoid <- countries[countries %in% f]
    print(paste0("Note: skipping countries already in Output: ", paste(avoid, collapse = ", ")))
    countries <- countries[!countries %in% f]
    
    if(!length(countries)){
      return(cat("All countries processed!"))
    }
    
  }
  
  file_api_base <- "https://dataverse.harvard.edu/api/access/datafile/"
  
  urls <- 
    data_df %>% 
    filter(country %in% countries) %>% 
    # The first USA sim files is empty, skip it!
    filter(!label %in% c("USA_200407")) %>% 
    group_by(country) %>% 
    slice(1:num_sims) %>% 
    ungroup() %>% 
    mutate(url = paste0(file_api_base, id)) %>% 
    data.frame()
  
  # Get data
  
  print("Start estimations from microdata...")
  
  gp <- find_grandparents2(urls, year, export)
  
  if(!export){
    return(gp)
  }
  
}

find_grandparents2 <- function(urls, year, export){
  
  gps_list <- 
    lapply(1:nrow(urls), function(n, urls, year){
      
      u <- urls$url[n]
      con <- urls$country[n]
      se <- urls$seed[n]
      lab <- urls$label[n]
      
      print(paste0("Loading data for ", lab))
      
      # Load data
      load(url(u))
      
      # Convert months to years
      opop <- 
        sims$opop %>% 
        mutate(
          ego_birth_year = asYr2(dob, FinalSimYear, endmo)
          , ego_death_year = asYr2(dod, FinalSimYear, endmo)
        )
      
      print("Finding grandparents...")
      out <- find_grandparents3(opop = opop, year = year)
      out$country <- con
      out$seed <- se
      # out$year <- year
      
      out$share <- out$gp / out$pop 
      out[is.na(out)] <- 0
      
      if(export){
        print("Writing grandparent estiamtes to Output.")
        write.csv(out, paste0("Output/gp_", lab, ".csv"))
      } else {
        return(out)
      }
      
      closeAllConnections()
      
    }, urls = urls, year = year)
  
  gps_df <-
    gps_list %>%
    bind_rows()
  
  return(gps_df)
  
}

# Function to identify share of the population that is a grandparent
find_grandparents3 <- function(opop, year){
  out_l <- 
    lapply(year, function(y, opop){
      print(paste0("Working on year ", y))
    # Keep only people alive in given year
    df_alive <- 
      opop %>% 
      filter(ego_birth_year <= y, ego_death_year > y) %>% 
      mutate(ego_age_now = y - ego_birth_year)
    
    # find grandparents of these people
    egos <- df_alive$pid
    match_rows <- match(egos, opop$pid)
    
    gp_pp <- opop$pop[match(opop$pop[match_rows], opop$pid)]
    gp_pm <- opop$pop[match(opop$mom[match_rows], opop$pid)]
    gp_mm <- opop$mom[match(opop$mom[match_rows], opop$pid)]
    gp_mp <- opop$mom[match(opop$pop[match_rows], opop$pid)]
    
    # ID of all grandparrents, doesn't matter if they're alive
    gp <- unique(c(gp_pp, gp_pm, gp_mm, gp_mp))
    
    # ID of living grandpanrents in year 'year'
    gp_alive <- gp[gp %in% egos]
    
    # Get number of grandpas by age
    gp_pop <- 
      df_alive[match(gp_alive, df_alive$pid), ] %>% 
      count(age = ego_age_now, name = "gp")
    
    # Get age distribution of population in general
    all_pop <- 
      df_alive %>% 
      count(age = ego_age_now, name = "pop")
    
    out <- 
      all_pop %>%
      left_join(gp_pop, by = c("age"))
    
    out$year <- y
    
    out
  }, opop)
  
  out <- bind_rows(out_l)
  out
  
}


# Get population data from UNWPP
get_unwpp_pop <- function(countries,  my_startyr = 2022, my_endyr = 2022){
  base_url <- 'https://population.un.org/dataportalapi/api/v1'
  
  # First, identify which indicator codes we want to use
  
  target <- paste0(base_url,'/indicators/?format=csv')
  codes <- read.csv(target, sep='|', skip=1) 
  
  pop_code <- codes$Id[codes$ShortName == "PopByAge1AndSex"]
  
  # Get location codes
  
  target <- paste0(base_url, '/locations?sort=id&format=csv')
  df_locations <- read.csv(target, sep='|', skip=1)
  
  # find the codes for countries
  iso3 <- countrycode(countries, origin = "country.name", destination = "iso3c")
  
  locs <- 
    df_locations %>% 
    filter(Iso3 %in% iso3) %>% 
    pull(Id) 
  
  my_location <- paste(locs, collapse = ",")
  
  print(paste0("Getting pop data for ", paste(countries, collapse = ", ")))
  
  
  # Avoid overwhelming UN APi
  if(length(countries) <= 20){
    
    my_indicator <- pop_code
    my_location  <- my_location
    
    target <- paste0(base_url,
                     '/data/indicators/',my_indicator,
                     '/locations/',my_location,
                     '/start/',my_startyr,
                     '/end/',my_endyr,
                     '/?format=csv')
    
    pop <- 
      read.csv(target, sep='|', skip=1) %>% 
      filter(Variant == "Median") %>% 
      select(iso3 = Iso3, country = Location, year = TimeLabel, age = AgeStart, sex = Sex, value = Value)
    
  } else{
    print("Many countries, I'll process in batch")
    
    my_indicator <- pop_code
    
    times <- floor(length(locs)/20)
    sp_vec <- rep(1:20, times)
    extras <- length(locs) - length(sp_vec)
    if(extras > 0) sp_vec <- c(sp_vec, 1:extras)
    
    my_location_l  <- split(locs, sp_vec)
    
    pop <- 
      lapply(1:length(my_location_l), function(n, my_location_l){
        
        print(paste0("Processing batch ", n, "/", length(my_location_l) ))
        
        loc_n <- paste(my_location_l[[n]], collapse = ",")
        
        target <- paste0(base_url,
                         '/data/indicators/',my_indicator,
                         '/locations/', loc_n,
                         '/start/',my_startyr,
                         '/end/',my_endyr,
                         '/?format=csv')
        
        pop <- read.csv(target, sep='|', skip=1)
        
        Sys.sleep(1)
        pop
      }, my_location_l) %>% 
      bind_rows() %>% 
      filter(Variant == "Median") %>% 
      select(iso3 = Iso3, country = Location, year = TimeLabel, age = AgeStart, sex = Sex, value = Value)
    
  }
  
  return(pop) 
  
}

# 2. Locate grandparents in simulaions -----------

data_df <- list_data()
all_countries <- unique(data_df$country)

# Get all countries 
if(countries[1] == "all"){
  print("Getting data for all UN countries.")
  # Remove regions, etc. 
  countries_df <- 
    data_df %>% 
    filter(!country %in% c("Chinaanddependencies", "ChinaMacaoSAR")) %>% 
    mutate(iso3 = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
    filter(!is.na(iso3))
  
  countries <- unique(countries_df$country)
}

# Get country iso codes
iso3_codes <- countrycode(countries, origin = "country.name", destination = "iso3c")

find_grandparents(countries, years, data_df, export = T)
Sys.time()

# Read gp data from disk

f <- list.files("Output", pattern = "gp_", full.names = T)

gps <- 
  lapply(f, read.csv) %>% 
  bind_rows() %>% 
  select(-X) %>% 
  rename(sim_pop = pop, sim_gp = gp, sim_share = share) %>% 
  mutate(iso3 = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  select(iso3, year, age, -country, everything())

# 3. Multiply by population numbers --------------

# Get pop numbers from UN
if(get_un_pop_from_api){
  if(!file.exists("un_pop.csv")){
    pop <- get_unwpp_pop(countries, my_startyr = min(year), my_endyr = max(year)) 
    write.csv(pop, "un_pop.csv", row.names = F)
  } else {
    pop <- read.csv("un_pop.csv", stringsAsFactors = F)
  }  
  # Aggregate by sex
  pop_un <- 
    pop %>% 
    arrange(iso3) %>% 
    filter(sex == "Both sexes") %>% 
    rename(pop_un = value) %>% 
    select(-country, -sex)
} else {
  if(!file.exists("Data/un_pop_full.csv")){
    # If this doesn't work, you may need to download the data from
    # https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip
    # https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.zip
    library(data.table)
    pop <- 
      fread("Data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv") %>% 
      bind_rows(fread("Data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.csv")) %>% 
      filter(LocTypeName == "Country/Area", Variant == "Medium") %>% 
      select(iso3 = ISO3_code, year = Time, age = AgeGrp, pop_un = PopTotal) %>% 
      mutate(
        age = as.numeric(ifelse(age == "100+", 100, age))
        , pop_un = pop_un*1000
        )
    
    fwrite(pop, "Data/un_pop_full.csv", row.names = F)
  } else {
  pop <- read.csv("Data/un_pop_full.csv", stringsAsFactors = F)  
  }
  pop_un <- 
    pop %>% 
    # Keep only relevant years and countries
    filter(year %in% years) %>% 
    filter(iso3 %in% iso3_codes)
}

# 4. Rough estimate of number of grandparents -------

# 4.1. By country ============

pop_gp_by_age <- 
  gps %>% 
  left_join(pop_un, by = c("iso3", "year", "age")) %>% 
  arrange(iso3) %>% 
  mutate(number_grandparents = sim_share * pop_un) %>% 
  select(iso3, year, age, number_grandparents, share_grandparents = sim_share, pop_un)

# Not by age, but for all ages combined
pop_gp <-
  pop_gp_by_age %>%
  group_by(iso3, year) %>%
  summarise(
    number_grandparents = sum(number_grandparents)
    , pop_un = sum(pop_un)
  ) %>%
  ungroup() %>%
  mutate(share_grandparents = number_grandparents/pop_un) %>%
  select(iso3, year, number_grandparents, share_grandparents, pop_un)


# 4.2. World ===========

pop_gp_world <-
  pop_gp %>%
  group_by(year) %>%
  summarise(
    number_grandparents = sum(number_grandparents)
    , pop_un = sum(pop_un)
  ) %>%
  ungroup() %>%
  mutate(share_grandparents = number_grandparents/pop_un)

# See coverage of estimates in terms of 'real' world population:

# World population in 2022
target <- paste0('https://population.un.org/dataportalapi/api/v1',
                 '/data/indicators/',49,
                 '/locations/',900,
                 '/start/',2022,
                 '/end/',2022,
                 '/?format=csv')

pop_world <- 
  read.csv(target, sep='|', skip=1) %>% 
  filter(Variant == "Median", Sex == "Both sexes") %>% 
  select(year = TimeLabel, value = Value)

# This is the proportion of completenes of gp estimates relative to 'real' world population
pop_gp_world[pop_gp_world$year == 2020,]$pop_un / pop_world$value

# 5. Export ----------
write.csv(pop_gp_by_age, "Output/grandparents_by_country_age.csv", row.names = F)
write.csv(pop_gp, "Output/grandparents_by_country.csv", row.names = F)
write.csv(pop_gp_world, "Output/grandparents_world.csv", row.names = F)

# 6. Plot -----------

# Distribution of grandparents over age 

pop_gp_by_age %>%
  filter(iso3 %in% c("CHN", "USA", "GTM")) %>% 
  filter(year == 2020) %>% 
  pivot_longer(number_grandparents:share_grandparents) %>% 
  ggplot(aes(x = age, y = value)) +
  geom_line() +
  geom_vline(xintercept = 65) +
  facet_wrap(name~iso3, scales = "free")

ggsave("Output/grandparents_age.pdf")

# Total grandparents around the world

pop_year <- pop_gp %>% 
  filter(year == 2020)

iso3_factor <- pop_year$iso3[order(pop_year$share_grandparents)]

pop_gp %>% 
  filter(year %in% c(1990, 2020)) %>% 
  mutate(iso3 = factor(iso3, levels = iso3_factor)) %>%
  pivot_longer(number_grandparents:share_grandparents) %>% 
  ggplot(aes(y = iso3, x = value)) +
  geom_col(position = position_dodge()) +
  facet_wrap(year~name, scale = "free") +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.text.y = element_text(angle = 30))

ggsave("Output/grandparents.pdf", height = 20, width = 12, units = "in")

# There should be linear relationship between # grandparents and # of 65+

pop_65 <- 
  pop_un %>% 
  filter(age >= 65) %>% 
  group_by(iso3, year) %>% 
  summarise(pop_un = sum(pop_un)) %>% 
  ungroup()

pop_gp %>% 
  select(iso3, year, number_grandparents) %>% 
  left_join(pop_65, by = c("iso3", "year")) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x = number_grandparents, y = pop_un, group = iso3, colour = year)) +
  geom_point() +
  geom_abline(slope = 1) +
  scale_x_log10("Number of grandparents in each country") +
  scale_y_log10("Number of 65+ in each country") +
  coord_equal() +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("Output/grandparents_vs_65+.pdf")

# Number of grandparents over time 

full_world <- 
  pop_gp_world %>% 
  left_join(
    pop_65 %>% 
      group_by(year) %>% 
      summarise(pop65 = sum(pop_un)) %>% 
      ungroup()
    , by = c("year")
    ) %>% 
  pivot_longer(-year) %>% 
  mutate(group = ifelse(name == "share_grandparents", "Grandparents per capita", "Number of people"))

full_world %>% 
  ggplot(aes(x = year, y = value, group = name, colour = name)) +
  geom_line(size = 1) +
  facet_wrap(.~group, scale = "free") +
  theme_bw() +
  labs(colour = "", y = "") +
  theme(legend.position = "bottom") 

ggsave("Output/grandparents_over_time.pdf")

# System info --------

Sys.info()

# sysname            release            version           nodename            machine              login               user 
# "Windows"           "10 x64"      "build 19044"       "LAP-404186"           "x86-64" "AlburezGutierrez" "AlburezGutierrez" 
# effective_user 
# "AlburezGutierrez" 