#
# R script to analyze spread of COVID-19, data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#

#
# load the required libraries
#
library(tidyverse)
library(lubridate)
source("config.inc")

# constant infinite
source("fitfunctions.r")

infinite = 10000
#
# import via web API
#
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
covid_raw <- read_csv(covidfile)


# process the time series into proper dataframe
covid_raw <- covid_raw %>% pivot_longer(!c("Province/State","Country/Region", "Lat","Long"), 
   names_to = "date",
   values_to = "infections")


# clean up column names 
colnames(covid_raw) = c("province","region","lat","long","date","infections")
covid_raw$date = as.Date(covid_raw$date, format="%m/%d/%y")
lastupdated = max(covid_raw$date)
covid_raw$time = covid_raw$date - min(covid_raw$date) + 1

# location assigments
locations = read_csv("sources/countrylist.csv")
covid <- covid_raw %>% left_join(locations) 
covid$location[is.na(covid$location)] = "Other"

# total spread of infections by countries
spread <- covid %>% group_by(date, time, location) %>% summarise(count=sum(infections)) %>% arrange(location, time)
spread$count[spread$count==0] = 1e-1

#
# clean up graphs and plot by country
#
capt = paste0(source, "\nlast updated:", format(lastupdated, format="%b %d, %Y"))

spread %>% ggplot + aes(date, count, color=location) + geom_point()  + 
                                        scale_y_log10(limit=c(1,1e8)) + 
                                        labs(caption=capt) + 
                                        xlab("Date") + ylab("Infections") + ggtitle("Global spread of COVID-19 infections by country") 


ggsave("graphs/covid-global-spread.pdf", device="pdf")
write_csv(spread, "data/covid-global-spread.csv")

# save data for later
countryinfections <- spread

#
#
# calculate daily growth by country
#
#
#

widespread <- spread %>% pivot_wider(names_from=location, values_from=count)
covid_growth <- as_tibble(lapply(widespread[,1:ncol(widespread)],diff,lag=1))
covid_growth$time = covid_growth$time + 1:NROW(covid_growth$time)


covid_growth <- covid_growth %>% pivot_longer(!c("time","date"),
   names_to = "location",
   values_to = "growth") #%>% filter(location!="Other")

covid_growth %>% mutate(date=as.Date("2020-01-22")+time) -> covid_growth

covid_growth %>% ggplot + aes(date, growth, color=location) + geom_line(linetype="longdash") + #geom_smooth(method="loess") +
                        #scale_x_continuous() + 
                        labs(caption=capt) + 
                        xlab("Date") + ylab("Growth of Infections") + ggtitle("Per diem growth of COVID-19 infections by country") +
                        facet_wrap(.~location)

ggsave("graphs/covid-global-spreadgrowth.pdf", device="pdf")
write_csv(covid_growth, "data/covid-global-spreadgrowth.csv")

# save data for later
countrygrowth <- covid_growth



#
# data for continents
#
#
# assign locations to differentiate between counntries/groups of countries
#
# continents = read_csv("sources/countries-by-continent.csv")
continents = read_csv("worldpopulation/countryinformation.csv") 
covid <- covid_raw %>% inner_join(continents) %>% rename(location = continent)
covid$location[is.na(covid$location)] = "Other"
#View(covid)

# total spread of infections by countries
spread <- covid %>% group_by(date, time, location) %>% summarise(count=sum(infections)) %>% arrange(location, time)
spread$count[spread$count==0] = 1e-1

#
# clean up graphs and plot
#

capt = paste0(source, "\nlast updated:", format(lastupdated, format="%b %d, %Y"))

spread %>%  filter(location != "Other") %>%
            ggplot + aes(date, count, color=location) + geom_point()  + 
                                        scale_y_log10(limit=c(1e3,1e7))  + labs(caption=capt) + 
                                        xlab("Date") + ylab("Infections") + ggtitle("Spread of COVID-19 infections by continent") 
                                        
 ggsave("graphs/covid-continents-spread.pdf", device="pdf")
 write_csv(spread, "data/covid-continents-spread.csv")

#
#
# calculate daily growth by continent
#
#
#

widespread <- spread %>% pivot_wider(names_from=location, values_from=count)
covid_growth <- as_tibble(lapply(widespread[,1:ncol(widespread)],diff,lag=1))
covid_growth$time = covid_growth$time + 1:NROW(covid_growth$time)


covid_growth <- covid_growth %>% pivot_longer(!c("time","date"),
   names_to = "location",
   values_to = "growth") #%>% filter(location!="Other")

covid_growth %>% mutate(date=as.Date("2020-01-22")+time) -> covid_growth

covid_growth %>%  filter(location !="Other") %>% filter(growth < 5e5) %>%
                  ggplot + aes(date, growth, color=location) + geom_line(linetype="longdash") + #geom_smooth(method="loess") +
                        #scale_x_continuous() + 
                        labs(caption=capt) + 
                        xlab("Date") + ylab("Growth of Infections") + ggtitle("Per diem growth of COVID-19 infections by continent") +
                        facet_wrap(.~location)

ggsave("graphs/covid-continents-spreadgrowth.pdf", device="pdf", width=11, height=8.5)
write_csv(covid_growth, "data/covid-continents-spreadgrowth.csv")

# save data for later
countrygrowth <- covid_growth

 