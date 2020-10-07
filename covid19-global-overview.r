#
# R script to analyze spread of COVID-19, data from Johns Hopkids via github
# Github: https://github.com/CSSEGISandData/COVID-19
#

#
# load the required libraries
#

library(tidyverse)
library(zoo)

deaths <- read_csv("data/covid-global-deathsgrowth.csv") %>% group_by(date) %>% summarize(deaths = sum(growth))
cases <- read_csv("data/covid-global-spreadgrowth.csv") %>% group_by(date) %>% summarize(cases = sum(growth))

casesdeaths <- deaths %>% inner_join(cases)

lastupdated = max(casesdeaths$date) 
capt = paste("Source: JHU\nlast updated:", lastupdated)

correction = 12
avdays = 7

totalcases = sum(casesdeaths$cases)
totaldeaths  = sum(casesdeaths$deaths)

casesdeaths %>% ggplot + aes(date, cases) + geom_line(color="blue", linetype="dotted") + geom_line(aes(y=rollmean(cases,avdays, na.pad=TRUE)), color="blue") + 
                        scale_y_continuous(sec.axis = sec_axis(~ ./correction, breaks=seq(0,100000,10000))) + #scale_y_log10(limit=c(10,100000))+ 
                        scale_x_date(date_breaks="1 month", date_labels = "%b %d") + 
                        labs(caption=capt) + xlab("Date") + ylab("Daily incremental number of confirmed cases or deaths") +
                        ggtitle(paste("Global daily cases and deaths with", avdays,"days average line")) + 
                        geom_line(aes(date, correction*deaths), color="red", linetype="dotted") + geom_line(aes(y=rollmean(correction*deaths,avdays,na.pad=TRUE)), color="red") +
                        annotate("text",x=as.Date("2020-03-10", format="%Y-%m-%d"),y=40000,label="cases\n<-----", color="blue") + 
                        annotate("text",x=as.Date("2020-04-08", format="%Y-%m-%d"),y=30000,label="deaths\n------>", color="red") +
                        annotate("text",x=as.Date("2020-02-28", format="%Y-%m-%d"),y=175000,label=paste("Total cases:", format(totalcases, big.mark=" ")), color="blue") + 
                        annotate("text",x=as.Date("2020-02-28", format="%Y-%m-%d"),y=150000,label=paste("Total deaths:", format(totaldeaths, big.mark=" ")), color="red") 

ggsave("graphs/covid-global-daily-cases-and-deaths.pdf", device="pdf")
write_csv(casesdeaths, "data/covid-global-daily-cases-and-deaths.csv")