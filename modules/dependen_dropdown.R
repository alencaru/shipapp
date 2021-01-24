#-------------------------------------------------------------------------------
#
# Module - dependent dropdown
#
#-------------------------------------------------------------------------------

dropdown_shipname_UI <- function(id, filter = NULL) {
  ns = NS(id)
 list(
    
      dropdown_input(ns("shipname")
                     , choices = NULL
                     , value = "NAN"
                     )

     
  )
}

dropdown_shipname <- function(input, output, session, data, input_shiptype) {
  
   ns <- session$ns

   output$dropdown <- renderText(input[["shipname"]])

    observe({
      ns <- session$ns
      update_dropdown_input(
             session
           , "shipname"
           , choices = unique(data()$SHIPNAME[data()$ship_type == input_shiptype()])
        ) 
      
      return(reactive({input$shipname}))

      })
     

}
  
