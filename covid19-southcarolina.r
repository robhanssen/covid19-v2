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

infinite = 10000
#
# import via web API
#
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
covid <- read_csv(covidfile) %>% select(-UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key, -Lat, -Long_)

# process the time series into proper dataframe
# covid <- melt(covid, id=c("Admin2", "Province_State","Country_Region"))
covid <- covid %>% pivot_longer(!c("Admin2", "Province_State","Country_Region"), 
   names_to = "date",
   values_to = "infections") %>% filter(Province_State=="South Carolina")


# clean up column names and differentiate between counties in SC Upstate
colnames(covid) = c("county","state","country","date","infections")
covid$date = as.Date(covid$date, format="%m/%d/%y")
lastupdated = max(covid$date)
covid$time = covid$date - min(covid$date) + 1

covid_fullstate <- covid # save full state info for later use

locations = read_csv("sources/SCcountylist.csv")
covid <- covid %>% right_join(locations) 
# covid$location[is.na(covid$location)] = "Other"

# total spread of infections by counties
spread <- covid %>% group_by(date, county, time) %>% summarise(count=sum(infections))

# spread_total <- covid %>% group_by(date, time) %>% summarise(count=sum(infections))
# spread_total$county = "Upstate"

# max_x = ceiling(max(spread$time)/10)*10
capt = paste("Source: JHU\nlast updated:", lastupdated)

widespread <- spread %>% pivot_wider(names_from=county, values_from=count)
covid_growth <- as_tibble(lapply(widespread[,1:ncol(widespread)],diff,lag=1))
covid_growth$time = covid_growth$time + 1:NROW(covid_growth$time)

covid_growth <- covid_growth %>% pivot_longer(!c("time","date"),
   names_to = "county",
   values_to = "cases") #%>% filter(location!="Other")

covid_growth %>% mutate(date=as.Date("2020-01-22")+time) -> covid_growth

infections <- covid_growth
#
# load the required libraries
#
# library(tidyverse)
# library(lubridate)

# constant infinite
# source("fitfunctions.r")

# infinite = 10000
#
# import via web API
#
#covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
covidfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
covid <- read_csv(covidfile) %>% select(-UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key, -Lat, -Long_)

# process the time series into proper dataframe
# covid <- melt(covid, id=c("Admin2", "Province_State","Country_Region")) %>% filter(variable != "Population")
covid <- covid %>% pivot_longer(!c("Admin2", "Province_State","Country_Region", "Population"), 
   names_to = "date",
   values_to = "deaths") %>% filter(Province_State=="South Carolina") %>% select(-Population)


# clean up column names and differentiate between counties in SC Upstate
colnames(covid) = c("county","state","country","date","deaths")
covid$date = as.Date(covid$date, format="%m/%d/%y")
lastupdated = max(covid$date)
covid$time = covid$date - min(covid$date) + 1

locations = read_csv("sources/SCcountylist.csv")
covid <- covid %>% right_join(locations) 

# total spread of infections by counties
spread <- covid %>% group_by(date, county, time) %>% summarise(count=sum(deaths))

widespread <- spread %>% pivot_wider(names_from=county, values_from=count)
covid_growth <- as_tibble(lapply(widespread[,1:ncol(widespread)],diff,lag=1))
covid_growth$time = covid_growth$time + 1:NROW(covid_growth$time)

covid_growth <- covid_growth %>% pivot_longer(!c("time","date"),
   names_to = "county",
   values_to = "deaths") #%>% filter(location!="Other")

covid_growth %>% mutate(date=as.Date("2020-01-22")+time) -> covid_growth

deaths <- covid_growth

# 
# 


# deaths <- deathsgrowth %>% group_by(date) %>% summarise(deaths=sum(growth))
# cases <- infectiongrowth %>% group_by(date) %>% summarise(cases=sum(growth))

casesdeaths <- deaths %>% inner_join(infections) #%>% group_by(date,time) %>% summarize(deaths=sum(deaths),cases=sum(cases))


correction = 14.5
avdays = 7

totalcases = sum(casesdeaths$cases)
totaldeaths  = sum(casesdeaths$deaths)

casesdeaths %>% ggplot + aes(date, cases) + geom_line(color="blue") + #geom_line(aes(y=rollmean(cases,avdays, na.pad=TRUE)), color="blue") + 
                        scale_y_continuous(sec.axis = sec_axis(~ ./correction, breaks=seq(0,100,10))) + #scale_y_log10(limit=c(10,100000))+ 
                        scale_x_date(date_breaks="3 months", date_labels = "%b %d") + 
                        facet_wrap(.~county, ncol=5) +
                        labs(caption=capt) + xlab("Date") + ylab("Daily incremental number of confirmed cases") +
                        ggtitle(paste("SC Upstate daily cases")) #+ 
                        #geom_line(aes(date, correction*deaths), color="red") #+ geom_line(aes(y=rollmean(correction*deaths,avdays,na.pad=TRUE)), color="red") #+
                        # annotate("text",x=as.Date("2020-03-15", format="%Y-%m-%d"),y=20000,label="cases\n<-----", color="blue") + 
                        # annotate("text",x=as.Date("2020-04-10", format="%Y-%m-%d"),y=10000,label="deaths\n------>", color="red") +
                        # annotate("text",x=as.Date("2020-02-28", format="%Y-%m-%d"),y=75000,label=paste("Total cases:", format(totalcases, big.mark=" ")), color="blue") + 
                        # annotate("text",x=as.Date("2020-02-28", format="%Y-%m-%d"),y=70000,label=paste("Total deaths:", format(totaldeaths, big.mark=" ")), color="red") 

write_csv(casesdeaths, "data/covid-southcarolina-growth.csv")
ggsave("graphs/covid-southcarolinaupstate.pdf", width=11, height=8)


#
# full state information on cases
#
#

SCpopulation = read_csv("sources/USpopulationregion.csv") %>% filter(state=="South Carolina") %>% select(population)

per100kfactor = 100000/SCpopulation$population

covid <- covid_fullstate # retrieve full state info
# View(covid_growth)
spread <- covid %>% group_by(date, time) %>% summarise(count=sum(infections))

# widespread <- spread %>% pivot_wider(!c("date","time"), values_from=count)
covid_growth <- as_tibble(lapply(spread[,2:3],diff,lag=1))
covid_growth$date = covid_growth$time + 1:NROW(covid_growth$time) + as.Date("2020-01-20")

covid_growth %>% ggplot + aes(date, count*per100kfactor) + geom_line(color="blue", alpha=.5, lty=2) + geom_line(aes(y=rollmean(count*per100kfactor,avdays, na.pad=TRUE)), color="blue") + 
                        scale_x_date(date_breaks="3 months", date_labels = "%b %d") + 
                        labs(caption=capt) + xlab("Date") + ylab("Daily incremental number of confirmed cases per 100k population") +
                        ggtitle(paste("South Carolina daily cases per 100k population")) #+ 

ggsave("graphs/covid-southcarolina.pdf", width=11, height=8)