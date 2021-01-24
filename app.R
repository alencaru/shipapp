library(shiny)
library(shiny.semantic)
library(geosphere)
library(tidyverse)
library(lubridate)
library(leaflet)
library(leaflet.extras2)
#-------------------------------------------------------------------------------

my_layout <- grid_template(default = list(
  areas = rbind(
    c("top"),
    c("middle"),
    c("filter")
  )
))

#-------------------------------------------------------------------------------
# modules ----

source("C:\\Users\\alencar\\Documents\\R_apps\\appsilon_ship_app\\modules\\dependen_dropdown.r")
#source("C:\\Users\\alencar\\Documents\\R_apps\\appsilon_ship_app\\modules\\leaf_module.r")

#-----------------------------
# tables

dTable <- readRDS(
  "C:\\Users\\alencar\\Documents\\R_apps\\shiny_ship_app\\shipapp\\data\\tab_a.rds")
ships_data <- readRDS(
  "C:\\Users\\alencar\\Documents\\R_apps\\shiny_ship_app\\shipapp\\data\\ships_data.rds")

countries <- readxl::read_excel("C:\\Users\\alencar\\Documents\\R_apps\\shiny_ship_app\\shipapp\\data\\countries_code.xlsx")


testthat::expect_is(dTable, "data.frame")
testthat::expect_is(ships_data, "data.frame")

dTable <- dTable %>% 
  left_join(ships_data) %>%
  left_join(countries) %>%
  as.data.frame(.) %>%
  mutate(year = year(DATETIME)
        ,month = month(DATETIME))

rm(ships_data)

#-------------------------------------------------------------------------------

ui <- semanticPage(
  margin = "5%",
  grid(my_layout,
       
       top = segment(
         div(style="display: inline-block;vertical-align:top; padding: 5px;"
             , img(src='logo3.jpg'
                   , style = "height: 58px; width: 58px; display: inline-block;")
         ),
         h3("SHIP RANGE DISTANCE MAP"
            , style = "display: inline-block; padding: 5px; text-align: center;")
         ), # end top
       
       #--------------------------
       middle = segment(

         leafletOutput("mymap")

         #leaf_module_UI("mymap")

       ), # end middle
       #--------------------------
       
       #--------------------------
       
       filter = segment(
         br(),
         fluidRow(  
           
           #dropdown_shipname_UI("shdrp", unique(dTable[["SHIPNAME"]])),
           
           div(
             
             div(
                 style="display: inline-block;vertical-align:top; width: 150px; margin: 15px;",
                 p("Ship type:"),
                 div(
                   dropdown_input( "shiptype"
                                   , choices = unique(dTable$ship_type)
                                   , value = "NAN"
                   ) # end dropdown1
                 )), # end div1
             
             div(
                 style="display: inline-block;vertical-align:top; width: 250px; margin: 15px;",
                 p("Ship name:"),
                 div(
                   dropdown_input( "shipname"
                                   , choices = NULL 
                                   , value = "NAN"
                   ) # end dropdown2
                   
                 )) # end div2 
             
           )# end fluid
         ) # end div
       ) # end segment
  ) # end grid
) # UI end


#-------------------------------------------------------------------------------
server <- function(input, output, session){
   
  # reactive_choose=reactive({
  #   input$shiptype
  # })
  #dt <- reactive({dTable})
  #dropname <- callModule(dropdown_shipname, "shdrp", dt, reactive_choose)
  #callModule(leafleft_module_server, "mymap", dt, reactive_choose, dropname)
  
  tags$div(tags$style(HTML("
                         #mymap {
                          padding: 0;
                          margin: 0;
                         }
                         #gtitle {
                          border: 1px;
                          margin: 5px;
                         }
                         .legend {
                         padding: 20px;
                         }

                         ")))

  
  output$dropdown <- renderText(input[["shiptype"]])
  output$dropdown <- renderText(input[["shipname"]])
  
  # Observe --------------------
  observe({
    
      shiny::validate(
      shiny::need(dTable,"Fetching data")
    )
    
    update_dropdown_input(session
                          , "shipname"
                          , choices = unique(dTable$SHIPNAME[dTable$ship_type == input$shiptype])
    ) # end dropdown
  }) # end of observe event

  
  #-----------------------------  
  # table
  table_reactive <- reactive({
    
    shiny::validate(
      shiny::need(input$shiptype,"Select shiptype"),
      shiny::need(dTable, "Waiting for data")
    )
 
    dTable %>%
      filter(ship_type == input$shiptype) %>%
      filter(SHIPNAME == input$shipname ) 

  })
  # 
  #-------------------------------------------------------------------------------
  #---- map note ----
  
  note <- reactive({
    paste0(
       "<strong>Vessel type: </strong>"
      , table_reactive()$ship_type, "<br>"
      , "<strong>Distance covered: </strong>"
      , as.integer(table_reactive()$distance*10)
      , " (meters)", "<br>"
      , "<strong> Country name: </strong>"
      , table_reactive()$country, "<br>" 
      , "<strong>Year: </strong>"
      , table_reactive()$year, "<br>"
      
      )
  })

  
  #-------------------------------------------------------------------------------
  # map
  output$mymap <- renderLeaflet({
    
    testthat::expect_is(table_reactive(), "data.frame")
    
  table_reactive() %>%
       leaflet() %>%
      addTiles() %>%
      addPolylines( ~c(LON, LON2), ~c(LAT, LAT2)
                   , weight = 2
                   , color = "red"
                   ) %>%
      addMarkers(
          ~LON
        , ~LAT
        , popup = paste0("<strong>LAT-LONG: </strong>"
                        , table_reactive()$LAT, " "
                        , table_reactive()$LON, "<br>")
        , label = paste0("Start point: ", "Click to see detailes")
        #, icon = point
       ) %>%
       addMarkers(~LON2
                , ~LAT2
                , popup = paste0(
                       "<strong>Distance (in meters): </strong>"
                , (table_reactive()$distance*10),"<br>"
                , "<strong>Date (inicial): </strong>"
                , table_reactive()$DATETIME, "<br>"
                , "<strong>LAT-LONG: </strong>"
                , table_reactive()$LAT2, " "
                , table_reactive()$LON2, "<br>"
               )
                , label = paste0("End point: ", "Click to see detailes")
                #, icon = ship_icon
      ) %>%
      addControl(note(), position = "topright")
  }) # map end
  #-----------------------------------------------------------------------------
  
  
} # server end

#-------------------------------------------------------------------------------
shinyApp(ui, server)
