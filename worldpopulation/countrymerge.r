library(tidyverse)

location <- read_csv("sources/countries-by-continent.csv")
population <- read_csv("sources/population.csv") %>% rename(region=country)


View(location)
View(population)

location %>% right_join(population) %>% View()
