library(tidyverse)
getwd()
gapminder_data <- read_csv("data/gapminder_data.csv")
summarize(gapminder_data, averageLifeExp=mean(lifeExp), medianLifExp=median(lifeExp))
#Learning to pipe

gapmider_summary <- gapminder_data %>% summarize(averageLifeExp=mean(lifeExp))
gapmider_summary

#Filtering

gapminder_data %>%
  filter(year == 1952) %>%
  summarize(average_gdp=mean(gdpPercap))
# Using group by
gapminder_data %>%
  group_by(year) %>%
  summarize(average=mean(lifeExp))

gapminder_data %>%
  group_by(year,continent) %>%
  summarize(average=mean(lifeExp), error = sd(lifeExp))

#Mutate Function
gapminder_data %>%
  mutate(gdp = pop * gdpPercap)

#Mutate a new cloumn with its population in millions

gapminder_data %>%
  mutate(popmillions = pop/100000)

#select
gapminder_data %>%
  select(pop, year)

gapminder_data %>%
  select(-continent)

#Pivot_wider

gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp )

#Working with messy data

read_csv("data/co2-un-data.csv", skip = 1)
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip=2,
                                col_names=c("region", "country", "year", "series", "value", "footnotes", "source"))
co2_emissions_dirty 

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% pivot_wider(names_from=series, values_from=value) %>% filter(year==2005) %>%
  select(-year)

co2_emissions
#Bringing in 2007 population data 

gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007) %>%
  select(-year,-continent)

inner_join(gapminder_data_2007, co2_emissions, by = "country")
anti_join(gapminder_data_2007, co2_emissions, by="country")
full_join(co2_emissions, gapminder_data_2007) %>% view 