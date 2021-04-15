#title: "Report_visulisation_2019"
#author: "B.Ndreka"
#date: "2021/04/14"

library(dplyr)
library(highcharter) 
# Set highcharter options
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
setwd("/Users/nbris/Desktop/group project/USCrime_STAT5225/Cleaned")
load("fbi_all.Rdata")
data_2019<-subset(state_rate_per_100k,Year=="2019")
head(data_2019)

hc_p <- data_2019[order(data_2019$PropertyCrime),] %>%
  hchart(
    "pyramid", hcaes(x = State, y =PropertyCrime),
                     name = "Property crime rate"
  )
hc_p
hc_v <- data_2019[order(data_2019$ViolentCrimeRevised),] %>%
  hchart(
    "pyramid", hcaes(x = State, y =ViolentCrimeRevised),
    name = "Volent crime rate"
  )
hc_v
