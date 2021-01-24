#--------------------------
# leaflet module

leaf_module_UI <- function(id){
  ns = NS(id)
  
  leafletOutput(ns("mymap"))
  
}

leafleft_module_server <- function(input, output, session, data, filter1, filter2) {

  
  note <- reactive({
    ns <- session$ns
    paste0(
    "<strong>Vessel type: </strong>", data()$ship_type, "<br>"
    ,"<strong>Distance covered: </strong>", as.integer(data()$distance*10), " (meters)", "<br>"
  )  })
  
   ns <- session$ns
  
  output$mymap <- renderLeaflet({
    ns <- session$ns
    data() %>% 
      filter(ship_type == filter1()) %>%
      filter(SHIPNAME == filter2()) %>%
    leaflet() %>%
    addTiles() %>%
    addPolylines(~c(LON, LON2), ~c(LAT, LAT2)) %>%
    addMarkers(
        ~LON
      , ~LAT
      , popup = paste0("<strong>Distance (in meters): </strong>", 0,"<br>")
      , label = paste0("Start point: ", "Click to see detailes")
    ) %>%
    addMarkers(  ~LON2
                 , ~LAT2
                 , popup = paste0(
                   "<strong>Distance (in meters): </strong>", (data()$distance*10),"<br>",
                   "<strong>Date (inicial): </strong>", data()$DATETIME, "<br>"
                 )
                 , label = paste0("End point: ", "Click to see detailes"
                 )
    ) %>%
    addControl(note(), position = "topright") 
  # addMeasure(
  #   position = "bottomleft",
  #   primaryLengthUnit = "meters",
  #   primaryAreaUnit = "sqmeters",
  #   activeColor = "#3D535D",
  #   completedColor = "#7D4479")
}) # map end

}