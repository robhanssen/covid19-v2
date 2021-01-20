library(tidyverse)
library(lubridate)

data <- read_csv("data/covid-us-daily-cases-and-deaths.csv") %>% 
                mutate(dow = weekdays(date),
                       weekdaygroup = ifelse(dow %in% c("Monday","Tuesday"), "Monday or Tuesday", "Other days")) %>%
                filter(date>"2020-06-30")

data %>% ggplot() + aes(date,deaths, color=weekdaygroup) + 
                geom_point() + 
                geom_smooth(method="loess", lty=2, se=FALSE) + 
                labs(x="Date", y="Daily deaths", color="Day of week")
