#title: "Report_visulisation_2019"
#author: "B.Ndreka"
#date: "2021/04/17"
library(tidyverse)
library(dplyr)
library(highcharter) 
library(broom)

setwd("C:\\Users\\admin\\Desktop\\USCrime_STAT5225-master")
load("fbi_with_gdp.Rdata")
#2019 data using crime rate
data_2019<-subset(state_gdp,Year=="2019")
head(data_2019)
#####map chart#######
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
crime_19<-area %>%
  filter(Year==2019)%>%
  select(State,PropertyCrime,Population )
attach(crime_19)
rate<-(PropertyCrime/Population)*100000
crime_19<-cbind(crime_19,rate)
# Load the world Map data
data(usgeojson, package = "highcharter")
hc <- highchart() %>%
  hc_add_series_map(
    usgeojson, crime_19, value = "rate", joinBy = c('name','State'),
    name = "Property crime rate"
  )  %>% 
  hc_colorAxis(stops = color_stops()) %>% 
  hc_title(text = "US Map") %>% 
  hc_subtitle(text = "Property crime rate in 2019")
hc
