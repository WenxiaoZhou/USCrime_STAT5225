### 2019 Violent Crime by State
library(flexdashboard)
library(shiny)
library(knitr)
library(tidyverse)
library(sp)
library(ggthemes)
library(leaflet)
library(DT)
library(plotly)
library(maps)
library(highcharter)
library(viridisLite)
library(forecast)
library(Hmisc)
library(highcharter)
setwd("/Users/zhouwenxiao/Desktop/USCrime")
load("fbi_with_gdp.RData")
thm1 <-
  hc_theme(
    colors = c("#90ed7d", "#434348","#1a6ecc"),
    chart = list(
      backgroundColor = "transparent",
      style = list(fontFamily = "Source Sans Pro")
    ),
    xAxis = list(
      gridLineWidth = 1
    )
  )


data("usgeojson")
heat2019<-state_gdp %>%
  filter(Year=="2019")
heat2019<-heat2019[,c(1:4,6,11)]
n <- 4
colstops <- data.frame(
  q = 0:n/n,
  c = substring(viridis(n + 1), 0, 7)) %>%
  list_parse2()
highchart() %>%
  hc_add_series_map(usgeojson,heat2019,name="Crime Rate",
                    value="ViolentCrime",
                    joinBy =c("woename","State"),
                    dataLabels=list(enabled=TRUE,
                                    format='{point.properties.postalcode}')) %>%
  hc_colorAxis(stops = colstops) %>%
  hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_add_theme(thm1) %>%
  hc_title(text='2019 Violent Crime Rate by States')



### 2019 Property Crime by State


highchart() %>%
  hc_add_series_map(usgeojson,heat2019,name="Crime Rate",
                    value="PropertyCrime",
                    joinBy =c("woename","State"),
                    dataLabels=list(enabled=TRUE,
                                    format='{point.properties.postalcode}')) %>%
  hc_colorAxis(stops = colstops) %>%
  hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_add_theme(thm1) %>%
  hc_title(text='2019 Property Crime Rate by States')

