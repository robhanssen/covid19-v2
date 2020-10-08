library(tidyverse)

countrylist <- read_csv("worldpopulation/countrylist.csv") %>% rename(region = list1)
population <- read_csv("worldpopulation/population.csv") 


x <- countrylist %>% left_join(population) %>% arrange(region)
View(x)
View(population %>% arrange(desc(population)))

View(countrylist)
View(location)
View(population)

write_csv(x, "worldpopulation/countryinformation.csv")