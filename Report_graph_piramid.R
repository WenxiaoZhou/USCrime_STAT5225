#title: "Report_visulisation_2019"
#author: "B.Ndreka"
#date: "2021/04/14"
library(tidyverse)
library(dplyr)
library(highcharter) 
library(broom)

setwd("C:\\Users\\admin\\Desktop\\USCrime_STAT5225-master")
load("fbi_with_gdp.Rdata")
#2019 data using crime rate
data_2019<-subset(state_gdp,Year=="2019")
head(data_2019)
# Set highcharter options
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
#property crime rate
hc_p <- data_2019[order(data_2019$PropertyCrime),] %>%
  hchart(
    "pyramid", hcaes(x = State, y =PropertyCrime,color=PropertyCrime ),
                     name = "Property crime rate"
  )
hc_p
#violent crime rate
hc_v <- data_2019[order(data_2019$ViolentCrimeRevised),] %>%
  hchart(
    "pyramid", hcaes(x = State, y =ViolentCrimeRevised, color=ViolentCrimeRevised),
    name = "Volent crime rate"
  )
hc_v
######### bobble_chart split by area & state########

#New York state/ size: controlled population, 
d_NY<- area %>%
  filter(State=="New York")
d_NY$Area<-as.factor(d_NY$Area)
head(d_NY)

bc<-d_NY%>%
  hchart(
    'scatter', hcaes(x =Year, y =ViolentCrimeLegacy, size =Population, group =Area),
    maxSize = "5%"
  )
bc





