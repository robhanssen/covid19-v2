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
locations = read_csv("sources/USstateslist.csv")

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

# total spread of infections by states
spread <- covid %>% group_by(date,time, location) %>% summarise(count=sum(infections))

max_x = ceiling(max(spread$time)/10)*10
capt = paste0(source, "\nlast updated:", format(lastupdated, format="%b %d, %Y"))

spread %>% mutate(date=as.Date("2020-01-22")-1+time) -> spread

spread %>% ggplot + aes(date, count, color=location) + geom_point() + geom_line() +
                        #scale_x_continuous(limit=c(40,max_x)) + 
                        scale_y_log10(limit=c(1e1,1e7)) + labs(caption=capt) + 
                        xlab("Date") + ylab("Number of cases per state") + ggtitle("Spread of COVID19 in the US by state") #+ 

ggsave("graphs/covid-us-spread.pdf", device="pdf")
write_csv(spread, "data/covid-us-spread.csv")

#
# daily growth of infections
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
                        xlab("Date") + ylab("Growth of Infections") + ggtitle("Per diem growth of COVID-19 infections") +
                        facet_wrap(.~location)

ggsave("graphs/covid-us-spreadgrowth.pdf", device="pdf")
write_csv(covid_growth, "data/covid-us-spreadgrowth.csv")

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
spread <- covid %>% group_by(date, time, location) %>% summarise(count=sum(deaths))

max_x = ceiling(max(spread$time)/10)*10

spread %>% ggplot + aes(time, count, color=location) + geom_point() + geom_line() +
                        scale_x_continuous(limit=c(40, max_x)) + scale_y_log10(limit=c(1e1,1e6)) + labs(caption=capt) + 
                        xlab("Days since Jan 22, 2020") + ylab("Number of casualties per state") + ggtitle("COVID19 casualties in the US by state")

ggsave("graphs/covid-us-deaths.pdf", device="pdf")
write_csv(spread, "data/covid-us-deaths.csv")

#
# daily increase of deaths in the US
#
#
widespread <- spread %>% pivot_wider(names_from=location, values_from=count)
covid_growth <- as_tibble(lapply(widespread[,1:ncol(widespread)],diff,lag=1))
covid_growth$time = covid_growth$time + 1:NROW(covid_growth$time)

covid_growth <- covid_growth %>% pivot_longer(!c("time","date"),
   names_to = "location",
   values_to = "growth") #%>% filter(location!="Other")

covid_growth %>% mutate(date = as.Date("2020-01-22")+time) -> covid_growth

covid_growth %>% ggplot + aes(date, growth, color=location) + geom_line(linetype="longdash") + #geom_smooth(method="loess") +
                        #scale_x_continuous() + 
                        labs(caption=capt) + 
                        xlab("Date") + ylab("Growth of Infections") + ggtitle("Per diem growth of COVID-19 casualties") +
                        facet_wrap(.~location)

ggsave("graphs/covid-us-deathsgrowth.pdf", device="pdf")
write_csv(covid_growth, "data/covid-us-deathsgrowth.csv")


# save variable for later use
deathsgrowth <- covid_growth


#
#
#
#
#
deaths <- deathsgrowth %>% group_by(date) %>% summarise(deaths=sum(growth))
cases <- infectiongrowth %>% group_by(date) %>% summarise(cases=sum(growth))

casesdeaths <- deaths %>% inner_join(cases)

correction = 60
avdays = 7

totalcases = sum(casesdeaths$cases)
totaldeaths  = sum(casesdeaths$deaths)
totalcasecomment = paste("Total cases: ", format(totalcases, big.mark=" "), "\nTotal deaths: ", format(totaldeaths, big.mark=" "), sep="")

casesdeaths %>% ggplot + aes(date, cases) + geom_line(color="blue", linetype="dotted") + geom_line(aes(y=rollmean(cases,avdays, na.pad=TRUE)), size=2, color="blue") + 
                        scale_y_continuous(sec.axis = sec_axis(~ ./correction, breaks=seq(0,6000,1000))) + #scale_y_log10(limit=c(10,100000))+ 
                        scale_x_date(date_breaks="1 month", date_labels = "%b %d") + 
                        labs(caption=capt) + xlab("Date") + ylab("Daily incremental number of confirmed cases or deaths") +
                        ggtitle(paste("US daily cases and deaths with", avdays,"days average line")) + 
                        geom_line(aes(date, correction*deaths), color="red", linetype="dotted") + geom_line(aes(y=rollmean(correction*deaths,avdays,na.pad=TRUE)), size=2, color="red") +
                        annotate("text",x=as.Date("2020-03-15", format="%Y-%m-%d"),y=20000,label="cases\n<-----", color="blue") + 
                        annotate("text",x=as.Date("2020-04-10", format="%Y-%m-%d"),y=10000,label="deaths\n------>", color="red") +
                        annotate("text",x=as.Date("2020-02-28", format="%Y-%m-%d"),y=175000,label=totalcasecomment, color="black")

ggsave("graphs/covid-us-daily-cases-and-deaths.pdf", device="pdf")
write_csv(casesdeaths, "data/covid-us-daily-cases-and-deaths.csv")