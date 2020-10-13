library(tidyverse)


# us file merge

fpop = "sources/USstatepopulation.csv"
fregion = "sources/USstateslist.csv"

pop = read_csv(fpop)
region = read_csv(fregion)

View(pop)
View(region)

states <- pop %>% full_join(region) %>% arrange(state) %>% write_csv("sources/USpopulationregion.csv")









read_USdata <- function(option)
{
    USdeathsfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
    USinfectionsfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"

    if(option=="infections") covidfile = USinfectionsfile
    else if(option=="deaths") covidfile = USdeathsfile

    covid <- read_csv(covidfile) %>% select(-UID, -iso2, -iso3, -code3, -FIPS, -Combined_Key, -Lat, -Long_)

    # process the time series into proper dataframe
    covid <- covid %>% pivot_longer(!c("Admin2", "Province_State","Country_Region"), 
                                        names_to = "date",
                                        values_to = "deaths")

    # clean up column names and differentiate between different regions
    colnames(covid) = c("county","state","country","date","deaths")
    covid$date = as.Date(covid$date, format="%m/%d/%y")
    lastupdated = max(covid$date)
    covid$time = covid$date - min(covid$date) + 1

    return(covid)
}

View(read_USdata("deaths"))

# 
#  US death data
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


#
#  US infection data
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
