library(DemoKin)
library(countrycode)
library(tidyverse)
library(httr)

# 0. Funciones --------

get_UNWPP_inputs <- function(countries, my_startyr, my_endyr, variant = "Median"){
  
  
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

# To get UN population
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
    
    times <- floor(length(locs)/10)
    sp_vec <- rep(1:10, times)
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

# 

# Parameters ----------
# pick countries
# countries <- c("China", "Guatemala", "Germany")
countries <- "all"

# Get all countries 
if(countries[1] == "all"){
  print("Getting data for all UN countries.")
un_countries <- read.csv("Data/un_countries.csv", stringsAsFactors = F)
}

# Year range

my_startyr   <- 1950
my_endyr     <- 2022

data <- get_UNWPP_inputs(
  countries = countries
  , my_startyr = my_startyr
  , my_endyr = my_endyr
  )


period_kin_temp <- 
  data %>%
  split(list(.$Location)) %>%
  map_df(function(X){
    print(unique(X$Location))
    U <-
      X %>%
      select(Time, age, px) %>%
      pivot_wider(names_from = Time, values_from = px) %>%
      select(-age) %>% 
      as.matrix()
    f <- X %>%
      select(Time, age, ASFR) %>%
      mutate(ASFR = ASFR/1000) %>% 
      pivot_wider(names_from = Time, values_from = ASFR) %>%
      select(-age) %>% 
      as.matrix()
    kin(U, f, time_invariant = FALSE, output_kin = c("gm","gd"), output_period = 2022)$kin_summary %>%
      mutate(Location = unique(X$Location),  .before = 1)
  })



period_kin <- 
  period_kin_temp %>% 
  select(Location, kin, year, age_focal, count_living) %>% 
  mutate(
    count_living = count_living*4
    , kin = ifelse(kin == "gm", "grandparents", "grandchildren")
    )



# 5. Number of grandparentes in a population 


pop <- 
  get_unwpp_pop(countries, my_startyr = 2022, my_endyr = 2022) %>% 
  filter(sex == "Both sexes") %>%
  rename(pop_un = value)

pp <- 
  period_kin %>% 
  rename(age = age_focal) %>% 
  mutate(iso3 = countrycode(Location, origin = "country.name", destination = "iso3c")) %>% 
  pivot_wider(names_from = kin, values_from = count_living) %>% 
  left_join(pop, by = c("iso3", "year", "age"))

# OPTION 2: get number of granchildren from average age of grandparents
# get mean number of grandchildren at each age
mean_gc <-
  period_kin_temp %>% 
  filter(kin == "gm") %>% 
  select(Location, year, age_focal, mean_age) %>% 
  mutate(
    mean_age = round(age_focal + mean_age, 0)
    , mean_age = ifelse(mean_age > 100 | is.na(mean_age), 100, mean_age)
  ) %>% 
  rename(age_gc = age_focal) %>% 
  left_join(
    period_kin_temp %>% 
      filter(kin == "gd") %>% 
      # To include male granchildren
      mutate(
        count_living = count_living*4
        # Since focal is included in the count, substract 1??
        # , count_living = count_living - 1
      ) %>% 
      select(Location, year, age_focal, number_gc = count_living)
    , by = c("Location", "year", "mean_age" = 'age_focal')
  ) %>% 
  rename(age_gp = mean_age) %>% 
  arrange(Location, year, age_gp) %>% 
  mutate(iso3 = countrycode(Location, origin = "country.name", destination = "iso3c")) %>% 
  select(-Location, - age_gp)


num_gp <-
  pp %>% 
  left_join(mean_gc, by = c("iso3", "year", "age" = "age_gc")) %>% 
  mutate(
    number_grandparents = pop_un*grandparents/number_gc
    , number_grandparents = ifelse(is.infinite(number_grandparents), 0, number_grandparents)
    , share_grandparents = number_grandparents/pop_un
  ) 

# Sum over all ages 

num_gp_sum <- 
  num_gp %>% 
  group_by(iso3, year) %>%
  summarise(
    number_grandparents = sum(number_grandparents)
    , pop_un = sum(pop_un)
  ) %>%
  ungroup() %>%
  mutate(share_grandparents = number_grandparents/pop_un) %>%
  select(iso3, year, number_grandparents, share_grandparents, pop_un)

num_gp_sum %>% 
  pivot_longer(number_grandparents:share_grandparents) %>% 
  ggplot(aes(y = iso3, x = value)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~name, scale = "free") +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.text.y = element_text(angle = 30))

print(num_gp_sum)


# Quality check: plot against number of 65+
  

pop_65 <- 
  pop %>% 
  filter(age >= 65) %>% 
  group_by(iso3, year) %>% 
  summarise(pop_un = sum(pop_un)) %>% 
  ungroup()

num_gp_sum %>% 
  select(iso3, year, number_grandparents) %>% 
  left_join(pop_65, by = c("iso3", "year")) %>% 
  ggplot(aes(x = number_grandparents, y = pop_un, group = iso3)) +
  geom_point() +
  geom_label(aes(label = iso3)) +
  geom_abline(slope = 1) +
  scale_x_log10("Number of grandparents") +
  scale_y_log10("Number of 65+") +
  coord_equal() +
  theme_bw()
