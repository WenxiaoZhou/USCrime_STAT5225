library(ggplot2)
library(plotly)
library(gapminder)
library(dplyr)
library(tidyverse)
library(Hmisc)

#Load data from Year 2005 to 2019
setwd("/Users/zhouwenxiao/Desktop/USCrime")
load("fbi_2005to2019.RData")


#(1)Bar plots in R Shiny
#Top 5 ViolentCrime states within years and time zones 
data_bar<-area_level_2005to2019[,c(1,2,3,4,5,6,11,15,16)]
pacific<-which(data_bar$State %in% capitalize(tolower(c("WASHINGTON","OREGON",
                          "CALIFORNIA","NEVADA","ALASKA","HAWAII"))))
mountain<-which(data_bar$State %in% capitalize(tolower(c("MONTANA","IDAHO",
                        "UTAH","ARIZONA","WYOMING","COLORADO","NEW MEXICO"))))
central<-which(data_bar$State %in% capitalize(tolower(c("NORTH DAKOTA","SOUTH DAKOTA",
                        "NEBRASKA","KANSAS","OKLAHOMA","TEXAS",
                        "MINNESOTA","IOWA","MISSOURI","ARKANSAS","LOUISIANA","WISCONSIN",
                        "ILLINOIS","TENNESSEE","MISSISSIPPI","ALABAMA"))))
index2<-c(pacific,mountain,central)
index1<-seq(1,dim(data_bar)[1],1)
eastern<-index1[!index1 %in% index2]
data_bar$index<-index1
data_bar$Section<-ifelse(data_bar$index %in% pacific,"Pacific",
                         ifelse(data_bar$index %in% mountain,"Mountain",
                                ifelse(data_bar$index %in% central,"Central","Eastern")))
data_bar$ViolentCrime1<-ifelse(data_bar$Population==0,0,ifelse(is.na(data_bar$ViolentCrimeRevised),
                              round(data_bar$ViolentCrimeLegacy/data_bar$Population,4),
                                      round(data_bar$ViolentCrimeRevised/data_bar$Population,4)))
                               
                               
data_bar$PropertyCrime1<-ifelse(data_bar$Population==0,0,round(data_bar$PropertyCrime/data_bar$Population,4))

#Interactive Bar plot--Violent Crime
#delete rows with NA in ViolentCrime or PropertyCrime
data_bar<-data_bar[-c(which(is.na(data_bar$ViolentCrime1)),which(is.na(data_bar$PropertyCrime1))),]
data_bar_vio<-data_bar[with(data_bar,order(Year,-ViolentCrime1)),]
data_bar_vio<-data_bar_vio[,c(1,2,3,5,11,12,13)]

library(shiny)
ui<-fluidPage(
  titlePanel("Top 5 states in ViolentCrime!"),
  sidebarLayout(
    sidebarPanel(
      selectInput('Section','Select time zone',selected='Pacific',
              choices=c("Pacific","Mountain","Central","Eastern")),
      sliderInput('Year','Select year',value=2005,min=2005,max=2019),
      selectInput('Area','Select an area type',selected='Metropolitan Statistical Area',
                  choices=c("Cities outside metropolitan areas","Nonmetropolitan counties",
                            "Metropolitan Statistical Area"))
      ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",plotOutput('plot_top_5_states_violent')),
        tabPanel("Table",tableOutput('table_top_5_states_details'))
     )
    )
  )
)
server<-function(input,output,session){
  output$plot_top_5_states_violent<-renderPlot({
    top_5_states_violent<-data_bar_vio %>%
      filter(Section==input$Section)  %>%
      filter(Year==input$Year)  %>%
      filter(Area==input$Area) %>%
      top_n(5,ViolentCrime1)
    ggplot(top_5_states_violent,aes(x=State,y=ViolentCrime1))+
      geom_col(fill='#263e63')
  })
  output$table_top_5_states_details<-renderTable({
    top_5_states_violent<-data_bar_vio %>%
      filter(Section==input$Section)  %>%
      filter(Year==input$Year)  %>%
      filter(Area==input$Area) %>%
      top_n(5,ViolentCrime1)
    round(top_5_states_violent[,c(6,7)],digits=4)
    top_5_states_violent
  })
}
shinyApp(ui=ui,server=server)









#(2) Radar chart
#change way to standardized, heatmap before radar chart to better intutive comparison
#between states
#https://www.r-graph-gallery.com/142-basic-radar-chart.html

transform <- function(col, maximum = 100) {
  lambda_list <- MASS::boxcox(col ~ 1, 
                              lambda = seq(-3, 3, 0.1), plotit = FALSE)
  lambda <- (lambda_list$x)[which.max(lambda_list$y)]
  if (near(lambda, 0)) {
    res <- log(col)
  } else {
    res <- (col^lambda - 1) / lambda
  }
  min <- min(res, na.rm = TRUE)
  max <- max(res, na.rm = TRUE)
  (res - min) / (max - min) * maximum
}

data_rad_std<-state_rate_per_100k_2005to2019 %>% 
  mutate(across(4:14, transform))
data_rad_std$State<-toupper(data_rad_std$State)

#Choose 7 columns of crime types
data_rad_std$Combinerape<-ifelse(is.na(data_rad_std$RapeLegacy),
                                 data_rad_std$RapeRevised,
                                 data_rad_std$RapeLegacy)
data_rad1<-data_rad_std[,c(1,2,5,7,8,10,11,12,15)]

#For some neighboring states, we explicitly want to compare their crime levels, 
#so we choose to use the radar chart below

library(ECharts2Shiny)
library(shiny)
CA_rad<-data_rad1[which(data_rad1$State=='CALIFORNIA'),]
NE_rad<-data_rad1[which(data_rad1$State=='NEVADA'),]
AR_rad<-data_rad1[which(data_rad1$State=='ARIZONA'),]
CA_tr<-CA_rad %>%
  filter(Year==2008) %>%
  select(Murder,Robbery,AggravatedAssault,Burglary,LarcenyTheft,MotorTheft,Combinerape) %>%
  t()
NE_tr<-NE_rad %>%
  filter(Year==2008) %>%
  select(Murder,Robbery,AggravatedAssault,Burglary,LarcenyTheft,MotorTheft,Combinerape) %>%
  t()
AR_tr<-AR_rad %>%
  filter(Year==2008) %>%
  select(Murder,Robbery,AggravatedAssault,Burglary,LarcenyTheft,MotorTheft,Combinerape) %>%
  t()
#dat<-data.frame(CA=CA_tr,NE=NE_tr,AR=AR_tr)
dat <- data.frame(CA = c(4300, 10000, 25000, 35000, 50000,25000,25000),
                  NE = c(5000, 14000, 28000, 31000, 42000,31000,31000),
                  AR = c(4000, 2000, 9000, 29000, 35000,35000,35000))
#names(dat)<-c("CALIFORNIA","NEVADA","ARIZONA")
row.names(dat)<-c("Murder","Robbery","AggrabatedAssault","Burglary","LarcenyTheft","MotorTheft","Rape")

server <- function(input, output) {
  renderRadarChart(div_id = "test",
                   data = dat)
}
ui <- fluidPage(
  loadEChartsLibrary(),
  
  tags$div(id="test", style="width:50%;height:400px;"),
  deliverChart(div_id = "test")
)

shinyApp(ui = ui, server = server)



#Comparison states that you choose within years
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
  sliderInput('Year','Select year',value=2010,min=2005,max=2019),
  plotOutput('plot_radar')
)
server<-function(input,output,session){
  output$plot_radar<-renderPlot({
    State1<-data_rad1 %>%
      filter(State==input$State1) %>%
      filter(Year==input$Year)
    State2<-data_rad1 %>%
      filter(State==input$State2) %>%
      filter(Year==input$Year)
    State3<-data_rad1 %>%
      filter(State==input$State3) %>%
      filter(Year==input$Year)
    radar<-data.frame(rbind(State1,State2,State3))
    radar<-radar[1:3,c(3:9)]
    row.names(radar)<-c("State1","State2","State3")
    radar<-rbind(rep(100,7),rep(0,7),radar)
    colors_border = c(rgb(0.2, 0.5, 0.5, 0.9),
                      rgb(0.8, 0.2, 0.5, 0.9) ,
                      rgb(0.7, 0.5, 0.1, 0.9))
    radarchart(radar,axistype=1,pcol=colors_border,
               plwd = 4,plty = 1,cglcol = "grey",cglty = 1,
               axislabcol = "grey",caxislabels = seq(0, 100, 10),
               cglwd = 0.8,vlcex = 0.8)
    legend(x = 1.5,y = 1,legend = rownames(radar[-c(1, 2),]),
           bty = "n",
           pch = 15 ,
           col=colors_border,
           text.col = "grey",
           cex = 1.2,
           pt.cex = 3)
    
  })
}
shinyApp(ui = ui, server = server)    











#(3) Animated Bubble Chart
#Method 1
#More data need for better visualization: population, GDP, relatioship to Crime counts
#https://towardsdatascience.com/create-hans-roslings-famous-animated-bubble-chart-in-a-single-piped-r-command-9c50a485259
library(viridis)
library(gapminder)
library(ggplot2)
library(gganimate)


data_bubb<-state_rate_per_100k_2005to2019 %>% 
  mutate(across(4:14, transform))
#data_bubb$State<-toupper(data_bubb$State)


pacific<-which(data_bubb$State %in% capitalize(tolower(c("WASHINGTON","OREGON",
                                                        "CALIFORNIA","NEVADA","ALASKA","HAWAII"))))
mountain<-which(data_bubb$State %in% capitalize(tolower(c("MONTANA","IDAHO",
                                                         "UTAH","ARIZONA","WYOMING","COLORADO","NEW MEXICO"))))
central<-which(data_bubb$State %in% capitalize(tolower(c("NORTH DAKOTA","SOUTH DAKOTA",
                                                        "NEBRASKA","KANSAS","OKLAHOMA","TEXAS",
                                                        "MINNESOTA","IOWA","MISSOURI","ARKANSAS","LOUISIANA","WISCONSIN",
                                                        "ILLINOIS","TENNESSEE","MISSISSIPPI","ALABAMA"))))
index2<-c(pacific,mountain,central)
index1<-seq(1,dim(data_bubb)[1],1)
eastern<-index1[!index1 %in% index2]
data_bubb$index<-index1
data_bubb$Section<-ifelse(data_bubb$index %in% pacific,"Pacific",
                         ifelse(data_bubb$index %in% mountain,"Mountain",
                                ifelse(data_bubb$index %in% central,"Central","Eastern")))

#Define a subset data includes Year, State, Section, ViolentCrime(total), PropertyCrime(total)
data_bubb$Violentcom<-ifelse(is.na(data_bubb$ViolentCrimeRevised),data_bubb$ViolentCrimeLegacy,
                             data_bubb$ViolentCrimeRevised)
data_bubb1<-data_bubb[,c(1:3,9,16,17)]
names(data_bubb1)<-c("State","Year","Population","PropertyCrime","Section","ViolentCrime")

data_bubb1$Year<-as.character(data_bubb1$Year)
ggplot(data_bubb1,aes(PropertyCrime,ViolentCrime,size=Population,color=Section))+
  geom_point(alpha = 0.5) + 
  scale_size(range = c(1,16), guide = FALSE) +    
  scale_x_continuous(limits = c(0, 100)) +  
  scale_y_continuous(limits = c(0, 100)) + 
  scale_color_viridis(discrete = TRUE, name = "Time Zone", option = "viridis") + 
  labs(x='Property Crime',y='Violent Crime') + 
  theme_classic() +  
  geom_text(aes(x =85, y = 100, label =Year),size = 11, 
            color = 'lightgrey',family="Times")+
  ggtitle("Relationship between population and ViolentCrime&PropertyCrime")+
  transition_states(Year,transition_length = 1,state_length = 1) +   
  ease_aes('cubic-in-out')
anim_save('bubbleplot.gif')


#Method 2
library(plotly)
library(gapminder)
p <- data_bubb1 %>%
  plot_ly(
    x = ~ViolentCrime, 
    y = ~PropertyCrime, 
    size = ~Population, 
    color = ~Section, 
    frame = ~Year, 
    text = ~State, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )
  )
p













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












