#' biologicScatter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 


    
xOptions <- get_options(tag = "contrast_", table = "mainTB")

yOptions <- get_options(tag = "contrast_", table = "mainTB")



mod_biologicScatter_ui <- function(id, title){
  ns <- NS(id)
  tabPanel(
    title = title, 
    sidebarPanel(
      shiny::helpText( "Help text" ),
      selectizeInput(
        ns("x_axis"), 
        label = "X axis",
        choices = xOptions[ 1 ],  
        selected = colnames( mtcars )[1]
      ),
      selectizeInput(
        ns("y_axis"), 
        label = "Y axis",
        choices = yOptions, 
        selected = yOptions[ 2 ]
      )
    ),
    mainPanel(
      plotOutput(ns("scatter1"))
    )
  )
}
    
#' biologicScatter Server Functions
#'
#' @noRd 
mod_biologicScatter_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    dfkey <- assembleKeyList()$dfkey
    
    dfData <- get_data()
    
    # observe({
    #     data <- get_data(
    #       table = "mainTB",
    #       colSel = c("gg_symbol", input$x_axis, input$y_axis)
    #     )
    # })
    
    output$scatter1 <- renderPlot({
      data <- get_data(
        table = "mainTB",
        colSel = c("gg_symbol", input$x_axis, input$y_axis)
      )
      
      p1 <- plotScatter(data = data, x_axis = input$x_axis, y_axis = input$y_axis)
      print(p1)
    })
 
  })
}
    
## To be copied in the UI
# mod_biologicScatter_ui("biologicScatter_1")
    
## To be copied in the server
# mod_biologicScatter_server("biologicScatter_1")
