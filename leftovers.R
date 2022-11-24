## B. Ratio granparents to grandchildren


This is a bit more tricky. I think 

$$
  \frac{\sum_{a=0}^{100}{\left[gp(a) \times w(a)\right]}}{\sum_{a=0}^{100}{\left[gc(a) \times w(a)\right]}}
$$
  
  
  ```{r}

# Get population numbers from UN

# 1x1 population of women
base_url <- 'https://population.un.org/dataportalapi/api/v1'

# First, identify which indicator codes we want to use

target <- paste0(base_url,'/indicators/?format=csv')
codes <- read.csv(target, sep='|', skip=1) 

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

print(paste0("Getting population data for ", paste(countries, collapse = ", ")))

my_indicator <- pop_code

target <- paste0(base_url,
                 '/data/indicators/',my_indicator,
                 '/locations/',my_location,
                 '/start/',my_startyr,
                 '/end/',my_endyr,
                 '/?format=csv')

pop <- 
  read.csv(target, sep='|', skip=1) %>%
  filter(Variant %in% variant) %>% 
  filter(Sex == "Female") %>% 
  select(Location, Time = TimeLabel, age = AgeStart, value = Value)

w <- 
  pop %>% 
  filter(Location == "China", Time %in% 2022) %>% 
  select(age, value) %>% 
  pull(value)

# Get kinship vectors
gp <- 
  period_kin %>% 
  filter(Location == "China", kin == "gm") %>% 
  select(age_focal, value = count_living) %>% 
  pull(value)

gc <- 
  period_kin %>% 
  filter(Location == "China", kin == "gd") %>% 
  select(age_focal, value = count_living) %>% 
  pull(value)

names(gp) <- names(gc) <- names(w) <- 0:100

```

First, let's try to weight the expected values of kin by population size:

$$
\sum{E[gp(a)] \times w(a)}
$$
```{r}
gpw <- gp * w
gcw <- gc * w

plot(gpw)
lines(gcw)

sum(gpw)/sum(gcw)

r <- gpw/gcw
r[is.infinite(r)] <- 0
plot(r)
sum(r)

```

I restrict this to age 55, since the expected number of grandparents for someone aged 50 is very very low. 

```{r}
period_kin %>% 
  pivot_wider(names_from = kin, values_from = count_living) %>% 
  filter(between(age_focal, 40, 50) ) %>% 
  mutate(
    ratio = gd/gm
    ) %>% 
  ggplot(aes(x = age_focal, y = ratio, color = Location)) +
  geom_line(size = 2) + 
  scale_x_continuous("Age of Focal (average member of the population)") +
  scale_y_continuous("Ratio of granchildren to grandmparents") +
  theme_bw() +
  theme(legend.position = "bottom")
```

## C. Number of granparents (per capita)