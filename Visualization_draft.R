library(ggplot2)
library(plotly)
library(gapminder)
library(dplyr)
library(tidyverse)
setwd("/Users/zhouwenxiao/Desktop/STAT5225-R and SAS Programming/Group Project/Data")
f1999<-read.csv("clean_1999.csv")
f2000<-read.csv("clean_2000.csv")
f2001<-read.csv("clean_2001.csv")
f2002<-read.csv("clean_2002.csv")
f2003<-read.csv("clean_2003.csv")
f2004<-read.csv("clean_2004.csv")
f2005<-read.csv("clean_2005.csv")

#add a year column
f1999$year<-rep(1999,dim(f1999)[1])
f2000$year<-rep(2000,dim(f2000)[1])
f2001$year<-rep(2001,dim(f2001)[1])
f2002$year<-rep(2002,dim(f2002)[1])
f2003$year<-rep(2003,dim(f2003)[1])
f2004$year<-rep(2004,dim(f2004)[1])
f2005$year<-rep(2005,dim(f2005)[1])

#Data combine
data_comb<-rbind(f1999,f2000,f2001,f2002,f2003,f2004)


#(1)Bar plots in R Shiny
#Top 5 ViolentCrime states within years and time zones 
data_bar<-data_comb[,c(1,2,3,4,12,13,14)]
#https://wenku.baidu.com/view/5bbeb3f16d85ec3a87c24028915f804d2a1687da.html
pacific<-which(data_bar$State %in% c("WASHINGTON","OREGON","CALIFORNIA","NEVADA","ALASKA","HAWAII"))
mountain<-which(data_bar$State %in% c("MONTANA","IDAHO","UTAH","ARIZONA","WYOMING","COLORADO","NEW MEXICO"))
central<-which(data_bar$State %in% c("NORTH DAKOTA","SOUTH DAKOTA","NEBRASKA","KANSAS","OKLAHOMA","TEXAS",
                                  "MINNESOTA","IOWA","MISSOURI","ARKANSAS","LOUISIANA","WISCONSIN",
                                  "ILLINOIS","TENNESSEE","MISSISSIPPI","ALABAMA"))
index2<-c(pacific,mountain,central)
index1<-seq(1,dim(data_bar)[1],1)
eastern<-index1[!index1 %in% index2]
data_bar$index<-index1
data_bar$Section<-ifelse(data_bar$index %in% pacific,"Pacific",
                      ifelse(data_bar$index %in% mountain,"Mountain",
                             ifelse(data_bar$index %in% central,"Central","Eastern")))
data_bar$ViolentCrime1<-round(data_bar$ViolentCrime/data_bar$Population,4)
data_bar$PropertyCrime1<-round(data_bar$PropertyCrime/data_bar$Population,4)


#Interactive Bar plot--Violent Crime
data_bar_vio<-data_bar[with(data_bar,order(year,-ViolentCrime1)),]
data_bar_vio<-data_bar_vio[,c(1,2,5,7,8,9,10)]
library(shiny)
ui<-fluidPage(
    titlePanel("Top 5 states in ViolentCrime!"),
    selectInput('Section','Select time zone',selected='Pacific',
                choices=c("Pacific","Mountain","Central","Eastern")),
    sliderInput('year','Select year',value=1999,min=1999,max=2004),
    selectInput('Area','Select an area type',selected='Rural',
                 choices=c("Cities outside metropolitan areas","Rural",
                           "Metropolitan Statistical Area")),
    plotOutput('plot_top_5_states')
)
server<-function(input,output,session){
  output$plot_top_5_states<-renderPlot({
    top_5_states<-data_bar_vio %>%
       filter(Section==input$Section)  %>%
      filter(year==input$year)  %>%
      filter(Area==input$Area) %>%
      top_n(5,ViolentCrime1)
    ggplot(top_5_states,aes(x=State,y=ViolentCrime))+
      geom_col(fill='#263e63')
  })
}
shinyApp(ui=ui,server=server)





#(2) Animated Bubble Chart
#Method 1
#More data need for better visualization: population, GDP, relatioship to Crime counts
library(gapminder)
library(ggplot2)
library(gganimate)
setwd("/Users/zhouwenxiao/Desktop/STAT5225-R and SAS Programming/Group Project")

data_bubb<-data_comb
pacific<-which(data_bubb$State %in% c("WASHINGTON","OREGON","CALIFORNIA","NEVADA","ALASKA","HAWAII"))
mountain<-which(data_bubb$State %in% c("MONTANA","IDAHO","UTAH","ARIZONA","WYOMING","COLORADO","NEW MEXICO"))
central<-which(data_bubb$State %in% c("NORTH DAKOTA","SOUTH DAKOTA","NEBRASKA","KANSAS","OKLAHOMA","TEXAS",
                                     "MINNESOTA","IOWA","MISSOURI","ARKANSAS","LOUISIANA","WISCONSIN",
                                     "ILLINOIS","TENNESSEE","MISSISSIPPI","ALABAMA"))
index2<-c(pacific,mountain,central)
index1<-seq(1,dim(data_bubb)[1],1)
eastern<-index1[!index1 %in% index2]
data_bubb$index<-index1
data_bubb$Section<-ifelse(data_bubb$index %in% pacific,"Pacific",
                         ifelse(data_bubb$index %in% mountain,"Mountain",
                                ifelse(data_bubb$index %in% central,"Central","Eastern")))
data_bubb_rural<-data_bubb[which(data_bubb$Area=='Rural'),]
data_bubb_rural$year<-as.integer(data_bubb_rural$year)
ggplot(data_bubb_rural,aes(Population,ViolentCrime,size=PropertyCrime,color=Section))+
  geom_point(alpha=0.7) +
  theme_bw()+
  labs(title='Year: {frame_time}',x='Population',y='Violent Crime')+
  transition_time(year)+
  ease_aes("linear")
anim_save('bubbleplot_sample.gif')


#Method 2
#https://towardsdatascience.com/create-hans-roslings-famous-animated-bubble-chart-in-a-single-piped-r-command-9c50a485259
library(viridis)
library(ggplot2)
library(gganimate)
data_bubb_rural$year<-as.character(data_bubb_rural$year)
ggplot(data_bubb_rural,aes(Population,ViolentCrime,size = PropertyCrime,color=Section))+
  geom_point(alpha = 0.5) + 
  scale_size(range = c(0.1,16), guide = FALSE) +    
  scale_x_continuous(limits = c(0, 2000000)) +  
  scale_y_continuous(limits = c(0, 7000)) + 
  scale_color_viridis(discrete = TRUE, name = "Time Zone", option = "viridis") + 
  labs(x='Population',y='Violent Crime') + 
  theme_classic() +  
  geom_text(aes(x = 1500000, y = 6000, label =year),size = 12, 
            color = 'lightgrey',family="Times")+
  ggtitle("Relationship between population and ViolentCrime&PropertyCrime")+
  transition_states(year,transition_length = 1,state_length = 1) +   
  ease_aes('cubic-in-out')






#(3) Radar chart
#change way to standardized, heatmap before radar chart to better intutive comparison
#between states
#https://www.r-graph-gallery.com/142-basic-radar-chart.html
data_rad_std<-data_comb
data_rad_std$Murder<-data_rad_std$Murder/data_rad_std$Population*1000
data_rad_std$Rape<-data_rad_std$Rape/data_rad_std$Population*1000
data_rad_std$Robbery<-data_rad_std$Robbery/data_rad_std$Population*1000
data_rad_std$AggravatedAssault<-data_rad_std$AggravatedAssault/data_rad_std$Population*1000
data_rad_std$Burglary<-data_rad_std$Burglary/data_rad_std$Population*1000
data_rad_std$LarcenyTheft<-data_rad_std$LarcenyTheft/data_rad_std$Population*1000
data_rad_std$MotorTheft<-data_rad_std$MotorTheft/data_rad_std$Population*1000
data_rad_std[,5:11]<-round(data_rad_std[,5:11],3)


#Comparison between closed states
library(shiny)
library(ECharts2Shiny)
CT_rad<-data_rad_std[which(data_rad_std$State=='CONNECTICUT'),]
MA_rad<-data_rad_std[which(data_rad_std$State=='MASSACHUSETTS'),]
NY_rad<-data_rad_std[which(data_rad_std$State=='NEW YORK'),]
CT_rural<-t(CT_rad[which(CT_rad$Area=='Rural'&CT_rad$year==2000),5:11])
MA_rural<-t(MA_rad[which(MA_rad$Area=='Rural'&MA_rad$year==2000),5:11])
NY_rural<-t(NY_rad[which(NY_rad$Area=='Rural'&NY_rad$year==2000),5:11])
dat_rad_com<-data.frame(CT=CT_rural,MA=MA_rural,NY=NY_rural)
names(dat_rad_com)<-c("CONNECTICUT",'MASSACHUSETTS','NEW YORK')
ui <- fluidPage(
  # We MUST load the ECharts javascript library in advance
  loadEChartsLibrary(),
  tags$div(id="test", style="width:50%;height:400px;"),
  deliverChart(div_id = "test")
)
server<-function(input, output) {
  renderRadarChart(div_id = "test",
                   data = dat_rad_com)
}
shinyApp(ui = ui, server = server)




#Comparison states that you choose, within years, area
data_rad1<-data_rad_std[which(data_rad_std$Area=='Metropolitan Statistical Area'),]
library(shiny)
library(fmsb)
library(shinydashboard)
ui<-fluidPage(
  titlePanel("States Crime level Comparison"),
  selectInput('State1','Select 1st state',selected='NEW YORK',
              choices=unique(data_rad1$State)),
  selectInput('State2','Select 2nd state',selected='MASSACHUSETTS',
              choices=unique(data_rad1$State)),
  selectInput('State3','Select 3rd state',selected='CONNECTICUT',
              choices=unique(data_rad1$State)),
  sliderInput('year','Select year',value=1999,min=1999,max=2004),
  plotOutput('plot_radar')
)
server<-function(input,output,session){
  output$plot_radar<-renderPlot({
    State1<-data_rad1 %>%
              filter(State==input$State1) %>%
              filter(year==input$year)
    State2<-data_rad1 %>%
      filter(State==input$State2) %>%
      filter(year==input$year)
    State3<-data_rad1 %>%
      filter(State==input$State3) %>%
      filter(year==input$year) 
    radar<-data.frame(rbind(State1,State2,State3))
    radar<-radar[1:3,c(5:11)]
    row.names(radar)<-c("State1","State2","State3")
    radar<-rbind(rep(50,7),rep(0,7),radar)
    colors_border = c(rgb(0.2, 0.5, 0.5, 0.9),
                      rgb(0.8, 0.2, 0.5, 0.9) ,
                      rgb(0.7, 0.5, 0.1, 0.9))
      radarchart(radar,axistype=1,pcol=colors_border,
                 plwd = 4,plty = 1,cglcol = "grey",cglty = 1,
                 axislabcol = "grey",caxislabels = seq(0, 50, 10),
                 cglwd = 0.8,vlcex = 0.8)
      legend(x = 0.7,y = 1,legend = rownames(radar[-c(1, 2),]),
        bty = "n",
        pch = 1 ,
        col = colors_in ,
        text.col = "grey",
        cex = 1.2,
        pt.cex = 3)
      
  })
}
shinyApp(ui = ui, server = server)    







#Heatmap on the US
#https://rpubs.com/jimu_xw/crime_visualization

library(maps)
statesMap<-map_data("state")
ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black")
#Make sure that the state names are the same in the crime data 
#frame and in the statesMap data frame. In crime: State are all
#Capital letters, in statesMap states are in region column are all lowercase.

#Subset
attach(f2004)
f2004_metro<-f2004[which(f2004$Area=="Metropolitan Statistical Area"),
                   c('State','Murder','Robbery','index','Section')]
f2004_metro$region<-tolower(f2004_metro$State)
#make sure our dataset contains more states than statesMap's regions
a<-unique(f2004_metro$region)
b<-unique(statesMap$region)

crimeMap<-inner_join(statesMap,f2004_metro,by="region")
ggplot(crimeMap, aes(x =long, y =lat, group = group, fill = Murder)) + 
  geom_polygon(color = "black") + scale_fill_gradient(low = "black", 
  high = "red", guide = "legend")+
  ggtitle("Heatmap of Murder cases in Metropolitan areas in Year 2004")
#better visualization with rate












