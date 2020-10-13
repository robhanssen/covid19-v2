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
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
covid_raw <- read_csv(covidfile)

countryinformation <- read_csv("worldpopulation/countryinformation.csv")

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

regionpopulation <- locations %>% inner_join(countryinformation) %>% group_by(location) %>% summarize(population=sum(population)) 

# total spread of infections by countries
spread <- covid %>% group_by(date, time, location) %>% summarise(count=sum(infections)) %>% arrange(location, time) %>% inner_join(regionpopulation)
spread$count[spread$count==0] = 1e-1
spread$per100k <- spread$count / spread$population * 100000
#
# clean up graphs and plot by country
#
capt = paste0(source, "\nlast updated:", format(lastupdated, format="%b %d, %Y"))

spread %>% ggplot + aes(date, per100k, color=location) + geom_point()  + 
                                        #scale_y_log10(limit=c(.1,1e3)) + 
                                        labs(caption=capt) + 
                                        xlab("Date") + ylab("Deaths") + ggtitle("Global COVID-19 casualties per 100k population") 


# ggsave("graphs/covid-global-deaths.pdf", device="pdf")
# write_csv(spread, "data/covid-global-deaths.csv")

# save data for later
countryinfections <- spread

# 
#  deaths growth by countrylist per 100k population
# 
# 

#regionpopulation <- countryinformation %>% group_by(continent) %>% summarise(population = sum(population)) %>% filter(!is.na(population))

covid <- covid_raw %>% left_join(locations) #%>% mutate(location=continent)
covid$location[is.na(covid$location)] = "Other"

# total spread of infections by countries
spread <- covid %>% group_by(date, time, location) %>% summarise(count=sum(infections)) %>% arrange(location, time)

widespread <- spread %>% pivot_wider(names_from=location, values_from=count)
covid_growth <- as_tibble(lapply(widespread[,1:ncol(widespread)],diff,lag=1))
covid_growth$time = covid_growth$time + 1:NROW(covid_growth$time)

covid_growth <- covid_growth %>% pivot_longer(!c("time","date"),
   names_to = "location",
   values_to = "growth") #%>% filter(location!="Other")

covid_growth %>% mutate(date=as.Date("2020-01-22")+time) %>%
                  #rename(continent = location) %>%
                  inner_join(regionpopulation) %>%
                  mutate(per100k = growth/population * 100000) %>%
                  filter(per100k >=0 )-> covid_growth

covid_growth %>% ggplot + aes(date, per100k, color=location) + geom_line(linetype="longdash") + #geom_smooth(method="loess") +
                        scale_y_continuous(limit=c(0,2.5)) + 
                        labs(caption=capt) + 
                        xlab("Date") + ylab("Growth of casualties per 100k population") + ggtitle("Per diem growth of COVID-19 casualties by country per 100k population") +
                        facet_wrap(.~location)









# 
# worst performing countries by death rate/100k in ordered bar graph 
# 

cutoffdate <- today() - days(1)
deathper100k <- covid_raw %>%  filter(date==cutoffdate) %>% 
                        group_by(region) %>% 
                        summarise(count=sum(infections)) %>% 
                        inner_join(countryinformation) %>% 
                        mutate(per100k=count/population*100000) %>% 
                        arrange(desc(per100k)) %>% filter(!is.na(per100k))
         
capt = paste0(source, "\nlast updated: ", format(cutoffdate, format="%b %d, %Y"))

# top30 = bind_rows(head(deathper100k,30),(deathper100k %>% filter(region=="Serbia")))

deathper100k %>% head(30)  %>% filter(population>100) %>% 
                  ggplot() + aes(x=reorder(region,per100k), y=per100k,fill=continent) + geom_bar(stat="identity") +
                              xlab("Country") + ylab("COVID-19 deaths per 100k population") +
                              coord_flip() + labs(caption=capt)

# 
#  deaths growth by continent per 100k population
# 
# 

regionpopulation <- countryinformation %>% group_by(continent) %>% summarise(population = sum(population)) %>% filter(!is.na(population))

covid <- covid_raw %>% left_join(countryinformation) %>% mutate(location=continent)
covid$location[is.na(covid$location)] = "Other"

# total spread of infections by countries
spread <- covid %>% group_by(date, time, continent) %>% summarise(count=sum(infections)) %>% arrange(continent, time)

widespread <- spread %>% pivot_wider(names_from=continent, values_from=count)
covid_growth <- as_tibble(lapply(widespread[,1:ncol(widespread)],diff,lag=1))
covid_growth$time = covid_growth$time + 1:NROW(covid_growth$time)

covid_growth <- covid_growth %>% pivot_longer(!c("time","date"),
   names_to = "location",
   values_to = "growth") #%>% filter(location!="Other")

covid_growth %>% mutate(date=as.Date("2020-01-22")+time) %>%
                  rename(continent = location) %>%
                  inner_join(regionpopulation) %>%
                  mutate(per100k = growth/population * 100000) %>%
                  filter(per100k >=0 )-> covid_growth

covid_growth %>% ggplot + aes(date, per100k, color=continent) + geom_line(linetype="longdash") + #geom_smooth(method="loess") +
                        scale_y_continuous(limit=c(0,1.5)) + 
                        labs(caption=capt) + 
                        xlab("Date") + ylab("Growth of casualties per 100k population") + ggtitle("Per diem growth of COVID-19 casualties by continent per 100k population") +
                        facet_wrap(.~continent)

ggsave("graphs/covid-continents-deaths-per-100k.pdf", width=11, height=8)