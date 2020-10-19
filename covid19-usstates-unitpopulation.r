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

# constant infinite
source("fitfunctions.r")
source("config.inc")
locations = read_csv("sources/USpopulationregion.csv")

infinite = 10000
#
# import via web API
#
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
covid <- read_csv(covidfile) %>% select(-UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key, -Lat, -Long_)

# process the time series into proper dataframe
covid <- covid %>% pivot_longer(!c("Admin2", "Province_State","Country_Region"), 
   names_to = "date",
   values_to = "infections")

# clean up column names and differentiate between regions
colnames(covid) = c("county","state","country","date","infections")
covid$date = as.Date(covid$date, format="%m/%d/%y")
lastupdated = max(covid$date)
covid$time = covid$date - min(covid$date) + 1

# selection of states to highlight out of the full data set
covid <- covid %>% left_join(locations) 
covid$location[is.na(covid$location)] = "Other"

# region population over US
regionpopulation <- locations %>% group_by(location) %>% summarize(population=sum(population)) 

# total spread of infections by states
spread <- covid %>% group_by(date,time, location) %>% summarise(count=sum(infections)) %>% inner_join(regionpopulation) %>% mutate(per100k = count/population*100000)

max_x = ceiling(max(spread$time)/10)*10
capt = paste0(source, "\nlast updated:", format(lastupdated, format="%b %d, %Y"))

spread %>% mutate(date=as.Date("2020-01-22")-1+time) -> spread

spread %>% ggplot + aes(date, per100k, color=location) + geom_point() + geom_line() +
                        #scale_x_continuous(limit=c(40,max_x)) + 
                        #scale_y_log10(limit=c(1e1,1e7)) + labs(caption=capt) + 
                        xlab("Date") + ylab("Number of cases per state") + ggtitle("Spread of COVID19 in the US by state") #+ 

# ggsave("graphs/covid-us-spread.pdf", device="pdf")
# write_csv(spread, "data/covid-us-spread.csv")

#
# daily growth of infections
#
widespread <- spread %>% select(-population,-per100k) %>% pivot_wider(names_from=location, values_from=count)
covid_growth <- as_tibble(lapply(widespread[,1:ncol(widespread)],diff,lag=1))
covid_growth$time = covid_growth$time + 1:NROW(covid_growth$time)

covid_growth <- covid_growth %>% pivot_longer(!c("time","date"),
   names_to = "location",
   values_to = "growth") #%>% filter(location!="Other")

covid_growth %>% mutate(date=as.Date("2020-01-22")+time) %>% inner_join(regionpopulation) %>%
                   mutate(per100k = growth/population*100000) -> covid_growth
capt=""
covid_growth %>% ggplot + aes(date, per100k, color=location) + geom_line(linetype="longdash") + #geom_smooth(method="loess") +
                        #scale_x_continuous() + 
                        labs(caption=capt) + 
                        xlab("Date") + ylab("Growth of Infections per 100k population") + ggtitle("Per diem growth of COVID-19 infections per 100k population") +
                        facet_wrap(.~location)

ggsave("graphs-unitpopulation/covid-us-spreadgrowth-per100k.pdf", device="pdf", width=11, height=8)
# write_csv(covid_growth, "data/covid-us-spreadgrowth.csv")

# save variable for later use
infectiongrowth <- covid_growth

#
# US deaths analysis
#
#
#

#
# import via web API
#
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
covid <- read_csv(covidfile) %>% select(-UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key, -Lat, -Long_, -Population)

# process the time series into proper dataframe
covid <- covid %>% pivot_longer(!c("Admin2", "Province_State","Country_Region"), 
   names_to = "date",
   values_to = "deaths")


# clean up column names and differentiate between different regions
colnames(covid) = c("county","state","country","date","deaths")
covid$date = as.Date(covid$date, format="%m/%d/%y")
lastupdated = max(covid$date)
covid$time = covid$date - min(covid$date) + 1

# selection of states to highlight out of the full data set
covid <- covid %>% left_join(locations) 
covid$location[is.na(covid$location)] = "Other"

# total spread of infections by states
spread <- covid %>% group_by(date, time, location) %>% summarise(count=sum(deaths)) %>% inner_join(regionpopulation) %>% mutate(per100k = count/population*100000)

max_x = ceiling(max(spread$time)/10)*10

spread %>% ggplot + aes(time, per100k, color=location) + geom_point() + geom_line() +
                        scale_x_continuous(limit=c(40, max_x)) + 
                        #scale_y_log10(limit=c(1e1,1e6)) + 
                        #labs(caption=capt) + 
                        xlab("Days since Jan 22, 2020") + ylab("Number of casualties per state") + ggtitle("COVID19 casualties in the US by state")

# ggsave("graphs/covid-us-deaths.pdf", device="pdf")
# write_csv(spread, "data/covid-us-deaths.csv")

#
# daily increase of deaths in the US
#
#
widespread <- spread %>% select(-population,-per100k) %>% pivot_wider(names_from=location, values_from=count)
covid_growth <- as_tibble(lapply(widespread[,1:ncol(widespread)],diff,lag=1))
covid_growth$time = covid_growth$time + 1:NROW(covid_growth$time)

covid_growth <- covid_growth %>% pivot_longer(!c("time","date"),
   names_to = "location",
   values_to = "growth") #%>% filter(location!="Other")

covid_growth %>% mutate(date = as.Date("2020-01-22")+time) %>% inner_join(regionpopulation) %>% mutate(per100k=growth/population*100000) -> covid_growth

covid_growth %>% ggplot + aes(date, per100k, color=location) + geom_line(linetype="longdash") + #geom_smooth(method="loess") +
                        #scale_x_continuous() + 
                        #labs(caption=capt) + 
                        xlab("Date") + ylab("Growth of Infections") + ggtitle("Per diem growth of COVID-19 casualties") +
                        facet_wrap(.~location)

ggsave("graphs-unitpopulation/covid-us-deathsgrowth-per100k.pdf", device="pdf", width=11, height=8)
# write_csv(spread, "data/covid-us-deathsgrowth.csv")


# save variable for later use
deathsgrowth <- covid_growth


#
#
#
#
#
deaths <- deathsgrowth %>% group_by(date) %>% summarise(deaths=sum(growth))
cases <- infectiongrowth %>% group_by(date) %>% summarise(cases=sum(growth))

totalpopulation = regionpopulation %>% summarise(population = sum(population))
as.numeric(totalpopulation[1])

casesdeaths <- deaths %>% inner_join(cases) %>%
                            mutate(population = as.numeric(totalpopulation[1]), deathsper100k=deaths/population*100000, casesper100k = cases/population*100000) %>%
                            mutate(deathrate = deaths/cases*100)
# View(casesdeaths)
correction = 14.5
avdays = 7


casesdeaths %>% ggplot + aes(date, casesper100k) + geom_line(color="blue", linetype="dotted") + geom_line(aes(y=rollmean(casesper100k,avdays, na.pad=TRUE)), color="blue") + 
                        scale_y_continuous(sec.axis = sec_axis(~ ./correction, breaks=seq(0,2,.2)), limit=c(0,25))+ 
                        scale_x_date(date_breaks="1 month", date_labels = "%b %d") + 
                        labs(caption=capt) + xlab("Date") + ylab("Daily incremental number of confirmed cases or deaths per 100,000") +
                        ggtitle(paste("US daily cases and deaths with", avdays,"days average line per 100k population")) + 
                        geom_line(aes(date, correction*deathsper100k), color="red", linetype="dotted") + geom_line(aes(y=rollmean(correction*deathsper100k,avdays,na.pad=TRUE)), color="red") +
                        geom_line(aes(date, deathrate), color="black", linetype="dotted") + geom_line(aes(y=rollmean(deathrate,avdays,na.pad=TRUE)), color="black") +
                        annotate("text",x=as.Date("2020-03-15", format="%Y-%m-%d"),y=6,label="cases\n<-----", color="blue") + 
                        annotate("text",x=as.Date("2020-04-16", format="%Y-%m-%d"),y=4,label="deaths\n------>", color="red")

# ggsave("graphs/covid-us-daily-cases-and-deaths.pdf", device="pdf")
# write_csv(casesdeaths, "data/covid-us-daily-cases-and-deaths.csv")