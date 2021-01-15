#
# R script to analyze spread of COVID-19, data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#

#
# load the required libraries
#
library(tidyverse)
library(lubridate)
library(zoo)
source("config.inc")

# constant infinite
source("fitfunctions.r")

infinite = 10000
#
# import via web API
#
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
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
# View(spread)
#
# clean up graphs and plot by country
#
capt = paste0(source, "\nlast updated:", format(lastupdated, format="%b %d, %Y"))

spread %>% ggplot + aes(date, per100k, color=location) + geom_point()  + 
                                        #scale_y_log10() + 
                                        labs(caption=capt) + 
                                        xlab("Date") + ylab("Infections") + ggtitle("Global spread of COVID-19 infections per 100k population") 


# ggsave("graphs/covid-global-spread.pdf", device="pdf")
# write_csv(spread, "data/covid-global-spread.csv")

# save data for later
countryinfections <- spread

cutoffdate <- today() - days(1)
infper100k <- covid_raw %>%  filter(date==cutoffdate) %>% 
                        group_by(region) %>% 
                        summarise(countinf=sum(infections)) %>% 
                        inner_join(countryinformation) %>% 
                        mutate(infper100k=countinf/population*100000) %>% 
                        arrange(desc(infper100k)) %>% filter(!is.na(infper100k))
         
capt = paste0(source, "\nlast updated: ", format(cutoffdate, format="%b %d, %Y"))

infper100k %>% head(30) %>% filter(population>1000000) %>%
                  ggplot() + aes(x=reorder(region,infper100k), y=infper100k,fill=continent) + geom_bar(stat="identity") +
                              xlab("Country") + ylab("COVID-19 infections per 100k population") +
                              coord_flip() + labs(caption=capt)

# 
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

# covid_growth %>% filter(continent =="Western Europe"|continent =="Eastern Europe"|continent =="Northern Europe"|continent =="North America") -> covid_growth
# capt=""
covid_growth %>% ggplot + aes(date, per100k, color=continent) + geom_line(linetype="longdash") + #geom_smooth(method="loess") +
                        scale_y_continuous(limit=c(0,100)) + 
                        labs(caption=capt) + 
                        xlab("Date") + ylab("Growth of infections per 100k population") + ggtitle("Per diem growth of COVID-19 infections by continent per 100k population") +
                        facet_wrap(.~continent)

ggsave("graphs-unitpopulation/covid-continents-infections-per-100k.pdf", width=11, height=8)

#
# try to make heat map
#
#
#  View(covid_growth)


# categorize impact levels 0-2: safe; 2-5: impacted; 5-10: severe; 10-20: critical

covid_growth1 <- covid_growth %>% mutate(week=week(date)) %>% group_by(continent, week) %>% summarize(date=date,per100k=mean(per100k)) %>% ungroup() %>%
                     mutate(levels = cut(per100k, breaks=c(-1,2,5,10,1e5),labels=c("Safe","Impacted","Severe","Critical")))
                                 

covid_growth1 %>% ggplot()  + aes(x=continent, y=date, fill=levels) + geom_tile() + coord_flip() + 
                   scale_fill_manual(values = c("Safe" = "darkgreen", "Impacted" = "yellow", "Severe"="orange","Critical"="red")) + 
                   scale_color_manual(values = c("Safe" = "darkgreen", "Impacted" = "yellow", "Severe"="orange","Critical"="red")) + 
                   labs(x="Continent", y="Date", caption="Safe: 0-2 per 100k\nImpacted: 2-5 per 100k\nSevere: 5-10 per 100k\n Critical: >10 per 100k")

ggsave("graphs-unitpopulation/covid19-continent-heat-map.pdf", width=11, height=8)