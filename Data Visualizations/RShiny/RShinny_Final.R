getwd
setwd("C:/Users/Gitesh/Desktop/Term 2/ALY6070/RShiny Project")
library(shiny)
library(data.table)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(rio)
library(ggthemes)
library(lubridate)
library(rworldmap)
library(knitr)
library(googleVis)
library(treemapify)


#importing police shooting dataset
data1 <-import("policeshooting.csv")


#formatting date in dataset
data1$date <- as.Date(data1$date, "%m/%d/%Y")

#changing Race as factor
data1$race <- as.factor(data1$race)

#changing State as factor
data1$state <- as.factor(data1$state) 

#chaning Gender as factor
data1$gender <- as.factor(data1$gender)

#Defining blank state as NA
levels(data1$race) <- sub("^$", "NA", levels(data1$race))

#Defining blank gender as NA
levels(data1$gender) <- sub("^$", "NA", levels(data1$gender))

#importing population by state dataset
data2<- import("popbystate.csv")  


#changing State as factor
data2$state <- as.factor(data2$state) 

#importing population by race
data3 <- import("popbyrace.csv")

#changing Race as factor
data3$race <- as.factor(data3$race)


#Count by race tibble
data1 %>% 
  filter(race!="NA") %>%
  group_by(race) %>% 
  summarise(count=n()) %>%  
  inner_join(., data3) %>% 
  mutate(rate_by_race = count/(Population/1000000)) -> RaceRate

#changing to dataframe
RaceRate <- as.data.frame(RaceRate)


#count by State tibble
data1 %>% 
  group_by(state) %>% 
  summarise(count1=n()) %>%  
  inner_join(., data2) %>% 
  mutate(rate_by_state = count1/(Pop2018/1000000)) -> StateRate

StateRate <- as.data.frame(StateRate)


#count by Year as tibble
countbyyear <- data1 %>% 
  mutate(year = year(date)) %>%
  group_by(year) %>% 
  summarise(count2=n()) 

#changing to data frame
YearCount <- as.data.frame(countbyyear)


#dashboard header for title
header <- dashboardHeader(title = "Fatal Shooting in the United States by a Police Officer in the line of Duty", titleWidth = 800)


#sidebar for dashboard
sidebar <-dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tablName ="dashboard", icon = icon("dashboard"))
  ), collapsed = TRUE
)



frow1 <- fluidRow(
  valueBoxOutput("value1", width = 2)
  ,valueBoxOutput("value2", width = 2)
  ,valueBoxOutput("value3", width = 2)
  ,valueBoxOutput("value4", width = 2)
  ,valueBoxOutput("value5", width = 2)
  ,valueBoxOutput("value6", width = 2)
  ,valueBoxOutput("value7", width = 2)
)


frow2 <- fluidRow(
  
  box(
    title = "Number of Fatality by Year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("yearbar", height = "300px")
  )
  
  ,box(
    title = "Rate by State per 1 Million Population"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,htmlOutput("stateplot", height = "300px")
  )
  
  ,box(
    title = "Rate by Race Per 1 Million Population"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("racerateoutput", height = "300px")
  ) 
  
  ,box(
    title = "Frequency distribution of Deceased by Age and Gender"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("AgeDistribution", height = "300px")
  )
  
)



# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='purple')



#Server for Dashboard
server <-function(input, output) {
  
#calculating for Value box
  total.count <- length(data1$id)
  
  gender.ratio <- paste(as.character(round(length(which(data1$gender == "M")) / length(which(data1$gender == "F")))), "1", sep = ":")
  
  no.body.cam <- round(100* length(which(data1$body_camera == FALSE)) / length(data1$body_camera),2)

  not.fleeing <- round(100 * length(which(data1$flee == "Not fleeing"))/ length(data1$flee),2) 
 
  mental.illness <- round(100* length(which(data1$signs_of_mental_illness == TRUE)) / length(data1$signs_of_mental_illness),2)
 
  not.threat <- round(100 * (1 - length(which(data1$threat_level == "attack"))/ length(data1$threat_level)),2) 
 
  unarmed <- round(100 * length(which(data1$armed == "unarmed"))/ length(data1$armed),2) 
 
 
 #valuebox output content
  
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total.count, format="d", big.mark=',')
      ,'Total Number of Fatality'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")
  })
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(gender.ratio, format="d")
      ,'Male to Female Ratio'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")
  })
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(no.body.cam, format="d")
      ,'Officer not Equipped with BodyCam %'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")
  })
  
  output$value4 <- renderValueBox({
    
    valueBox(
      formatC(not.fleeing, format="d")
      ,'Subjects Not Fleeing from Officer %'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")
  })
  
  output$value5 <- renderValueBox({
    
    valueBox(
      formatC(mental.illness, format="d")
      ,'Subjects with History of Mental illness %'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")
  })
  
  output$value6 <- renderValueBox({
    
    valueBox(
      formatC(not.threat, format="d")
      ,'Officer not under threat %'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")
  })
  

  
#output content for box  
  agedist <- ggplot(data1 , aes(x = data1$age, fill = data1$gender)) +
    geom_histogram(bins = 12, position = "dodge") + ylab("Number of Fatality ") + 
    xlab("Age of Deceased ")+
    ggtitle("")
  
  options(scipen=999)
  

  output$AgeDistribution <- renderPlot({agedist})
  
  
  #Output of tree map
  #raceheat <- treemap(RaceRate, index="race", vSize="rate_by_race", type="index")
  racetree <- ggplot(RaceRate, aes(area= rate_by_race, label= race, fill= rate_by_race)) + 
    geom_treemap() +
    scale_fill_gradient(low = "green", high = "red") +
    geom_treemap_text(fontface = "bold", colour= "black", place = "centre", grow = TRUE)
  
  output$racerateoutput <- renderPlot(racetree)
    
  
  
  #Out for Rate by State map
  output$stateplot <- renderGvis({
    map<-gvisGeoChart(StateRate, "state", "rate_by_state",
                      options=list(region="US", 
                                   displayMode="regions", 
                                   resolution="provinces",displayMode="text",
                                   width=800, height=300, 
                                   colorAxis="{minValue:0, colors:[\'#10C200\', \'#D63A3A\']}"
                                   )) 
    
        return(map) 
  })
  
  
  horbar <- ggplot(YearCount, aes(x= year, y= count2), colours= "blue")  +
    geom_bar(stat="identity", col= "black") + coord_flip()
    horbar 
  
    
  output$yearbar <- renderPlot(horbar)
  
      
}

shinyApp(ui, server) 



     