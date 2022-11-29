
# Answer question 'how many grandparents there are in the world'
# I'll try answering this with simulation now using the SOCSIM outputs from the paper:
# Alburez‐Gutierrez, D., Mason, C., and Zagheni, E. (2021). The “Sandwich Generation” Revisited: Global Demographic Drivers of Care Time Demands. Population and Development Review 47(4):997–1023. doi:10.1111/padr.12436.

# 0. Preamble ------
# To get data from Harvard datavesre
# https://cran.r-project.org/web/packages/dataverse/vignettes/C-download.html
# library("dataverse")

library(tidyverse)
library(httr)

countries <- c("Germany", "Guatemala")
# How many simulations per country? max 5
num_sims <- 1

# 1. Get links to download simulation data from Harvard Dataverse --------
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SSZL6U

# Get list of files from Datavsers

file_api_base <- "https://dataverse.harvard.edu/api/access/datafile/"
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

urls <- 
  data_df %>% 
  filter(country %in% countries) %>% 
  group_by(country) %>% 
  slice(1:num_sims) %>% 
  ungroup() %>% 
  mutate(url = paste0(file_api_base, id))

# 2. Define function to find grandparents

u <- urls$url[1]
y <- load(url(u))

qq <- load("Afghanistan_205969.rsave")


x <- load(url("https://dataverse.harvard.edu/api/access/datafile/4656065"))
download.file("https://dataverse.harvard.edu/api/access/datafile/4656065", destfile = "x.rsave")
load("x.rsave")
