How many grandparents are there in the world?
================
Dec 07 2022

  - [1. Load packages and functions](#1-load-packages-and-functions)
  - [2. Example for one country: grandparents in
    Guatemala](#2-example-for-one-country-grandparents-in-guatemala)
  - [3. Replicate for all countries](#3-replicate-for-all-countries)

|                                                                         |
| :---------------------------------------------------------------------- |
| ***Code by***                                                           |
| Diego Alburez-Gutierrez, PhD                                            |
| Kinship Inequalities Research Group,                                    |
| Max Planck Institute for Demographic Research                           |
| <https://www.demogr.mpg.de/en/research_6120/kinship_inequalities_10703> |
| alburezgutierrez\[at\]demogr.mpg.de                                     |
| <https://www.twitter.com/d_alburez>                                     |

These are back-of-the-envelope estimates to answer the question: ‘how
many grandparents are there in the world’? I do this by relying on a
series of country-level synthetic population microdata with a
genealogical structure produced using the SOCSIM software
(<https://lab.demog.berkeley.edu/socsim/>). The synthetic microdata come
from the paper:

Alburez‐Gutierrez, D., Mason, C., and Zagheni, E. (2021). The “Sandwich
Generation” Revisited: Global Demographic Drivers of Care Time Demands.
Population and Development Review 47(4):997–1023. doi:
<https://doi.org/10.1111/padr.12436>.

Details on the simulation implementation, data sources, and parameters
are given in the paper. Here I will analyse the replication data for
this paper, which is stored in the Harvard Dataverse:
<https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SSZL6U>.

Note that the estimates are rough approximations and estimates for
smaller countries (e.g., with populations smaller than 1 million) should
be interpreted with special care.

The scripts access these data using the Harvard Dataverse API, so you
need a connection to the internet to replicate this code. The code runs
in R, preferably in RStudio.

# 1\. Load packages and functions

## 1.1. Packages

``` r
library(tidyverse)
library(httr)
library(countrycode)
library(data.table)
library(knitr)
```

## 1.2. Functions

Define a number of functions for getting data, re-arranging simulation
data, and doing the analysis.

<details>

<summary><b>SHOW CODE</b></summary>

<p>

``` r
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
```

</p>

</details>

# 2\. Example for one country: grandparents in Guatemala

First, show how the estimation works for Guatemala as an example. Later
on, I’ll scale this up for all world countries.

Define parameters for analysis:

``` r
countries <- c("Guatemala")
# How many simulations per country? max 5
# Don't touch this
num_sims <- 1

years <- 2022

# Don't touch this
get_un_pop_from_api <- T

# To convert months to yeares in SOCSIM
FinalSimYear <-  2200
endmo <-  5400
```

Load the simulation data from the Harvard database

``` r
# Show simulations for which countries are available
data_df <- list_data()
```

    ## [1] "Getting API ready..."

``` r
all_countries <- unique(data_df$country)

# Get country iso codes
iso3_codes <- countrycode(countries, origin = "country.name", destination = "iso3c")
```

Now, implement the analysis to see how many people have grandparents in
the simulation

``` r
gps_df <- find_grandparents(countries, years, data_df, export = F)
```

    ## [1] "Start estimations from microdata..."
    ## [1] "Loading data for Guatemala_296608"
    ## [1] "Finding grandparents..."
    ## [1] "Working on year 2022"

``` r
# rename things
gps <-  
  gps_df %>% 
  rename(sim_pop = pop, sim_gp = gp, sim_share = share) %>% 
  mutate(iso3 = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  select(iso3, year, age, -country, everything())
```

Combine with UN population numbers to get an approximation of the number
of grandparents in the real population.

``` r
# Get pop numbers from UN using API
pop <- get_unwpp_pop(countries, my_startyr = years, my_endyr = years)
```

    ## [1] "Getting pop data for Guatemala"

``` r
# Aggregate by sex
pop_un <- 
    pop %>% 
    arrange(iso3) %>% 
    filter(sex == "Both sexes") %>% 
    rename(pop_un = value) %>% 
    select(-country, -sex)

# Combine simulation estimates and UN population numbers to get estimated number of grandparents

pop_gp_by_age <- 
  gps %>% 
  left_join(pop_un, by = c("iso3", "year", "age")) %>% 
  mutate(number_grandparents = sim_share * pop_un) %>% 
  select(iso3, year, age, number_grandparents, share_grandparents = sim_share, pop_un)
```

We can now visualise the number of grandparents over age and the ‘per
capita’ number of grandparents over age (i.e., the share of the
population aged `x` who are grandparents).

``` r
# Distribution of grandparents over age 

pop_gp_by_age %>%
  rename(`Grandparents per capita` = share_grandparents, `Number of grandparents` = number_grandparents) %>% 
  select(-pop_un) %>% 
  pivot_longer(-c(iso3, year, age)) %>% 
  # mutate() %>% 
  ggplot(aes(x = age, y = value)) +
  geom_line() +
  facet_wrap(.~name, scales = "free") +
  labs(y = "") +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Finally, let’s answer the question: how many grandparents are there in
Guatemala in 2022, irrespective of how old the are?

``` r
# Not by age, but for all ages combined
  pop_gp_by_age %>%
  group_by(iso3, year) %>%
  summarise(
    number_grandparents = sum(number_grandparents)
    , pop_un = sum(pop_un)
  ) %>%
  ungroup() %>%
  mutate(share_grandparents = number_grandparents/pop_un) %>%
  mutate(
    pop_un = round(pop_un/1e6, 1)
    , number_grandparents = round(number_grandparents/1e6, 1)
    ) %>% 
  select(iso3, year, `Number of grandparents (millions)` = number_grandparents, `Total population (millions)` = pop_un, `Grandparents per capita` = share_grandparents) %>% 
  kable()
```

    ## `summarise()` has grouped output by 'iso3'. You can override using the `.groups`
    ## argument.

| iso3 | year | Number of grandparents (millions) | Total population (millions) | Grandparents per capita |
| :--- | ---: | --------------------------------: | --------------------------: | ----------------------: |
| GTM  | 2022 |                               2.9 |                        17.8 |               0.1615601 |

# 3\. Replicate for all countries

We do the same thing, but scaling it up to all countries present in the
UNWPP data (<https://population.un.org/wpp/>).

Note that running the code chunk below is likely to take a couple of
hours and requires constantly downloading data from the Harvard
Dataverse and writing outputs to the disk. The final estimates are
stored in the [Output](Output) directory, at the country level (by age
and for all ages combined) and for the entire world. Estimates for
1990-2020 rely on UNWPP ‘historical’ data, whereas estimates for
2025-2040 come from UNWPP projections (medium scenario).

``` r
# 1. Preamble

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

# 2. Locate grandparents in simulaions -----------

data_df <- list_data()
all_countries <- unique(data_df$country)

# Get all countries 
print("Getting data for all UN countries.")
# Remove regions, etc. 
countries_df <- 
    data_df %>% 
    filter(!country %in% c("Chinaanddependencies", "ChinaMacaoSAR")) %>% 
    mutate(iso3 = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
    filter(!is.na(iso3))
  
countries <- unique(countries_df$country)

# Get country iso codes
iso3_codes <- countrycode(countries, origin = "country.name", destination = "iso3c")

find_grandparents(countries, years, data_df, export = T)

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
if(!file.exists("Data/un_pop_full.csv")){
    # If this doesn't work, you may need to download the data from
    # https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip
    # https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.zip
  
  # Download UN data and unzip

  # Estimates and historical data
  
  # download.file("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip", "Data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip")
  
  unzip("Data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip", exdir = "Data")
  
  file.remove("Data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip")
  
  # Projections
  
    # download.file("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.zip", "Data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.zip")
  
  unzip("Data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.zip", exdir = "Data")
  
  file.remove("Data/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.zip")

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

# For the whole world
pop_gp_world <-
  pop_gp %>%
  group_by(year) %>%
  summarise(
    number_grandparents = sum(number_grandparents)
    , pop_un = sum(pop_un)
  ) %>%
  ungroup() %>%
  mutate(share_grandparents = number_grandparents/pop_un)

# 5. Export 
write.csv(pop_gp_by_age, "Output/grandparents_by_country_age.csv", row.names = F)
write.csv(pop_gp, "Output/grandparents_by_country.csv", row.names = F)
write.csv(pop_gp_world, "Output/grandparents_world.csv", row.names = F)
```

## Session info

``` r
sessionInfo()
```

    ## R version 4.0.2 (2020-06-22)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19044)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.1252 
    ## [2] LC_CTYPE=English_United Kingdom.1252   
    ## [3] LC_MONETARY=English_United Kingdom.1252
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] knitr_1.31        data.table_1.14.0 countrycode_1.2.0 httr_1.4.2       
    ##  [5] forcats_0.5.1     stringr_1.4.0     dplyr_1.0.5       purrr_0.3.4      
    ##  [9] readr_1.4.0       tidyr_1.1.3       tibble_3.1.0      ggplot2_3.3.3    
    ## [13] tidyverse_1.3.0  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.0 xfun_0.21        haven_2.3.1      colorspace_2.0-0
    ##  [5] vctrs_0.4.1      generics_0.1.0   htmltools_0.5.2  yaml_2.2.1      
    ##  [9] utf8_1.2.1       rlang_1.0.2      pillar_1.5.1     withr_2.5.0     
    ## [13] glue_1.6.2       DBI_1.1.1        dbplyr_2.1.0     modelr_0.1.8    
    ## [17] readxl_1.3.1     lifecycle_1.0.0  munsell_0.5.0    gtable_0.3.0    
    ## [21] cellranger_1.1.0 rvest_1.0.0      evaluate_0.17    labeling_0.4.2  
    ## [25] fastmap_1.1.0    curl_4.3         fansi_0.4.2      highr_0.8       
    ## [29] broom_0.7.5      Rcpp_1.0.7       backports_1.2.1  scales_1.1.1    
    ## [33] jsonlite_1.7.2   farver_2.1.0     fs_1.5.0         hms_1.0.0       
    ## [37] digest_0.6.28    stringi_1.5.3    grid_4.0.2       cli_3.2.0       
    ## [41] tools_4.0.2      magrittr_2.0.1   crayon_1.4.1     pkgconfig_2.0.3 
    ## [45] ellipsis_0.3.2   xml2_1.3.2       reprex_1.0.0     lubridate_1.7.10
    ## [49] assertthat_0.2.1 rmarkdown_2.7    rstudioapi_0.13  R6_2.5.0        
    ## [53] compiler_4.0.2
