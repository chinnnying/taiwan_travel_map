#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(vistime)
library(highcharter)
library(leaflet)

table_data <- read_csv("all.csv")
cities <- unique(table_data$Region)
cities <- cities[! cities %in% c(NA)]
result_table <- data.frame(Type = c("Scenic Spots","Events","Restaurants","Accomodations"),
                           n = c(0,0,0,0))

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Taiwan Tourism Map"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("house")),
      menuItem("Calendar of events", tabName = "calendar", icon = icon("calendar-days")),
      menuItem("Images provided by owners", tabName = "images", icon = icon("images")),
      menuItem("Search by keywords", tabName = "search", icon = icon("magnifying-glass")),
      menuItem("About this project", tabName = "about", icon = icon("info")))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
      
    fixedRow(
      # A static valueBox
      valueBoxOutput("scenicBox", width = 3),
      valueBoxOutput("restaurantBox", width = 3),
      valueBoxOutput("hotelBox", width = 3),
      valueBoxOutput("eventBox", width = 3)
    ),
    fixedRow(

      column(9, leafletOutput("map", height = "60vh", width = "100%")),
      column(3, tabBox(height = "60vh", width="90%",
                       tabPanel("Overview", 
                                h4("Welcome to Taiwan Tourism Map! This project is based on the combination of 4 open datasets provided by Taiwan Tourism Bureau. Hope to help you discover hidden gems in Taiwan & plan the trip easier than ever!"),
                                h6("**Click the marker to see the description. Explore now!"),
                                br(),
                                br(),
                                br(),
                                textOutput("click_name"),
                                textOutput("click_text")),
                       tabPanel("Filter", h4("Select the options based on your preference!"),
                                checkboxGroupInput("itype", label=h4("Info types you'd like to view"),
                                            choices = c('Restaurants', 'Events', 'Scenic Spots', 'Accomodations'),
                                            selected = c('Restaurants', 'Events', 'Scenic Spots', 'Accomodations')),
                                selectInput("iplace", label=h4("Places you'd like to visit"),
                                            choices= cities, multiple = TRUE,
                                            selected = cities)
                                #dateInput("idate", label=h4("Date you're planning to go"), value = "2022-01-01")
                                )
                    )),
    ),
    fixedRow(
      column(12, dataTableOutput('dto'))
      
    )
    ),
    #------------------------------calendar-------------------------------------
    tabItem(tabName = "calendar", highchartOutput("gantt")),
    #------------------------------images---------------------------------------
    tabItem(tabName = "images"),
    #------------------------------search---------------------------------------
    tabItem(tabName = "search"),
    #------------------------------about----------------------------------------
    tabItem(tabName = "about",
            fluidRow(
              box( title = "About this project",
                   status = "primary",
                   width = "6 col-lg-4",
                   p("This project collected Taiwan tourism open data and using 
                     r shiny to create a web app to achieve interactive data visualization.
                     The purpose of this project is to provide an easy-to-use platform for
                     anyone who is interested in exploring Taiwan. The data includes information
                     of restaurants, events, hotels and scenic spots in Taiwan. With a leaflet map,
                     users can easily explore Taiwan simply by clicking the markers to learn more about
                     the open hour, introduction, and phone number. Images of the location will show if
                     it is provided. The app also provides custom search function. Users can filter out
                     the information they are not interested in so they can plan their trip faster. Hope
                     you find this app helpful :)"),
                   p("This project is built in R with shiny and leaflet."),
                   h4("About me"),
                   p("I am a data science student from Monash University in Australia. This is a self-motivated project 
                     for myself to practice data cleaning and data visualization."))
            
            ))
) # tab-items close
  
) # dash-body close
) # ui close


server <- function(input, output, session) {
 
  
  
  ######### value boxes ##########################
  output$scenicBox <- renderValueBox({
    valueBox(
      paste0(value_sce), "Scenic Spots to explore", icon = icon("mountain-sun"),
      color = "purple"
    )
  })
  output$restaurantBox <- renderValueBox({
    valueBox(
      paste0(value_res), "Restaurants", icon = icon("utensils"),
      color = "yellow"
    )
  })
  output$hotelBox <- renderValueBox({
    valueBox(
      paste0(value_acc), "Accomodation around", icon = icon("bed"),
      color = "maroon"
    )
  })
  output$eventBox <- renderValueBox({
    valueBox(
      paste0(value_eve), "Events Coming", icon = icon("binoculars"),
      color = "green"
    )
  })
  ######### leaflet map ##########################
  output$map <- renderLeaflet({
    leaflet(table_data) %>% 
      addTiles() %>%
      setView(lat=24, lng=121 , zoom=7) %>%
      addProviderTiles("OpenStreetMap") %>%
      addMarkers(
        lng = table_data$Px,
        lat = table_data$Py,
        #radius =3,
        #color = "yellow",
        popup = paste("<h4>",
                      table_data$Name,
                      "</h4>",
                      "<h5>(",
                      table_data$Type,
                      ")</h5>",
                      "<img src=",
                      table_data$Picture1,
                      " width=200 alt='Image not provided'><br>",
                      "Phone: ",
                      table_data$Tel,
                      "<br>",
                      "Address: ",
                      table_data$Add,
                      "<br>",
                      "Open Hour: ",
                      table_data$Opentime,
                      "<br>",
                      "Website: <a href=",
                      table_data$Website,
                      ">",
                      table_data$Website,
                      "</a><br>"
        ),
        clusterOptions = markerClusterOptions()
      )
    
  })
  observe({
    # get info of user click
    click <- input$map_marker_click
    if(is.null(click))
      return()
    # get country lat and lon
    clng <- click$lng
    clat <- click$lat
    # filter data based on click
   name <- table_data %>% filter(Py == clat) %>% filter(Px == clng) %>% select(Name) %>% head(1)
   describe <- table_data %>% filter(Py == clat) %>% filter(Px == clng) %>% select(Description) %>% head(1)
    #cname <- paste(describe[1])
    # -----cuntry name box--------------------------------------------------------------
    output$click_text <- renderText({
      paste(describe[1])
    })
    output$click_name <- renderText({
      paste(name[1])
   }) 

  })
  ######### check boxes ##########################
  
  ######### table ##########################

  
  bottom_table <- table_data %>% select(Type, Name, Tel, Add, Description, Region)
  result <- bottom_table %>% count(Type)
  value_table <- rows_update(result_table, result)
  value_acc <- value_table %>% filter(Type=="Accomodations") %>% select(n)
  value_sce <- value_table %>% filter(Type=="Scenic Spots") %>% select(n)
  value_eve <- value_table %>% filter(Type=="Events") %>% select(n)
  value_res <- value_table %>% filter(Type=="Restaurants") %>% select(n)
  
  
  output$dto <- renderDataTable({
    bottom_table %>% filter(Type %in% input$itype) %>% filter(Region %in% input$iplace) 
    }, options = list(pageLength = 5)
    )
  
  observe({
    map_data <- table_data %>% filter(Type %in% input$itype) %>% filter(Region %in% input$iplace)
    result <- bottom_table %>% filter(Type %in% input$itype) %>% filter(Region %in% input$iplace)%>% count(Type)
    value_table <- rows_update(result_table, result)
    value_acc <- value_table %>% filter(Type=="Accomodations") %>% select(n)
    value_sce <- value_table %>% filter(Type=="Scenic Spots") %>% select(n)
    value_eve <- value_table %>% filter(Type=="Events") %>% select(n)
    value_res <- value_table %>% filter(Type=="Restaurants") %>% select(n)
    ######### value boxes ##########################
    output$scenicBox <- renderValueBox({
      valueBox(
        paste0(value_sce), "Scenic Spots to explore", icon = icon("mountain-sun"),
        color = "purple"
      )
    })
    output$restaurantBox <- renderValueBox({
      valueBox(
        paste0(value_res), "Restaurants", icon = icon("utensils"),
        color = "yellow"
      )
    })
    output$hotelBox <- renderValueBox({
      valueBox(
        paste0(value_acc), "Accomodation around", icon = icon("bed"),
        color = "maroon"
      )
    })
    output$eventBox <- renderValueBox({
      valueBox(
        paste0(value_eve), "Events Coming", icon = icon("binoculars"),
        color = "green"
      )
    })
    output$map <- renderLeaflet({
      leaflet(map_data) %>% 
        addTiles() %>%
        setView(lat=24, lng=121 , zoom=7) %>%
        addProviderTiles("OpenStreetMap") %>%
        addMarkers(
          lng = map_data$Px,
          lat = map_data$Py,
          #radius =3,
          #color = "yellow",
          popup = paste("<h4>",
                        map_data$Name,
                        "</h4>",
                        "<h5>(",
                        map_data$Type,
                        ")</h5>",
                        "<img src=",
                        map_data$Picture1,
                        " width=200 alt='Image not provided'><br>",
                        "Phone: ",
                        map_data$Tel,
                        "<br>",
                        "Address: ",
                        map_data$Add,
                        "<br>",
                        "Open Hour: ",
                        map_data$Opentime,
                        "<br>",
                        "Website: <a href=",
                        map_data$Website,
                        ">",
                        map_data$Website,
                        "</a><br>"
          ),
          clusterOptions = markerClusterOptions()
        )
      
    })
    
  })

  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
