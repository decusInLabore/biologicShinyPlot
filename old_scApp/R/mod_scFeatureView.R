#' scFeatureView UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @import DBI
#' @import RMySQL
#' @import ggplot2
#' @import colourpicker
#' @import scales
#' @import ggridges
#' @import colourpicker
#'
#' @importFrom shiny NS tagList 
mod_scFeatureView_ui <- function(id, title){
    ns <- NS(id)
    
    ## Load parameters ##
    startUpList <- golem::get_golem_options( which = "startUpList" )
    geneDefault <- startUpList$keyList$geneDefault
    dropDownList <- startUpList$utilityList$dropDownList
    numCols <- startUpList$utilityList$numCols
    nonNumCols <- startUpList$utilityList$nonNumCols
    splitOptions <- startUpList$utilityList$dropDownList$splitByColumn$selOptions
    
  tabPanel(
    title = title, 
    # Sidebar panels
    sidebarPanel(
      ## Item sidepanel
      helpText(
        paste0(
          "To create a Violin Plot plot, select, for example, as x-axis: seurat clusters, as y-axis: log10Expr and as colorBy: seurat clusters. \n\n To view averaged expression values for signature gene categories, start typing cat_ in the search box to see category suggestions. "
        )
      ),
      ## Item sidepanel
      conditionalPanel(
        condition = paste0(
          'input[\'', ns('colorBy'), "\'] == \'lg10Expr\' || input[\'", ns('x_axis'), "\'] == \'lg10Expr\' || input[\'", ns('y_axis'), "\'] == \'lg10Expr\'  "
        ),
        
        selectizeInput(
          ns("gene"), 
          label = as.vector(dropDownList[["gene"]][["displayName"]]),
          choices = NULL, 
          options = list(maxOptions = 50))
      ),
      ## Item sidepanel
      selectInput(
        ns("x_axis"),
        label = as.vector(dropDownList[["x_axis"]][["displayName"]]),
        choices =dropDownList[["x_axis"]][["selOptions"]],
        selected = as.vector(dropDownList[["x_axis"]][["default"]])
      ),
      ## Item sidepanel
      selectInput(
        ns("y_axis"),
        label = as.vector(dropDownList[["y_axis"]][["displayName"]]),
        choices =dropDownList[["y_axis"]][["selOptions"]],
        selected = as.vector(dropDownList[["y_axis"]][["default"]])
      ),
      ## Item sidepanel
      selectInput(
        ns("splitByColumn"),
        label = as.vector(dropDownList[["splitByColumn"]][["displayName"]]),
        choices =dropDownList[["splitByColumn"]][["selOptions"]],
        selected = as.vector(dropDownList[["splitByColumn"]][["default"]])
      ),
      ## Item sidepanel
      selectInput(
        ns("colorBy"),
        label = as.vector(dropDownList[["colorBy"]][["displayName"]]),
        choices =dropDownList[["colorBy"]][["selOptions"]],
        selected = as.vector(dropDownList[["colorBy"]][["default"]])
      ),
      
      ## Item sidepanel
      conditionalPanel(
        condition = paste0(paste0('input[\'', ns('colorBy'), "\'] == \'",numCols,"\'"), collapse = " || "),
        colourpicker::colourInput(ns("dotcolor"), "Select High Color", "darkblue"),
        colourpicker::colourInput(ns("lowColor"), "Select Low color", "#D3D3D3")
      ),
      ## Item sidepanel
      conditionalPanel(
        condition = paste0(paste0('input[\'', ns('colorBy'), "\'] == \'",nonNumCols,"\'"), collapse = " || "),
        #condition = paste0("input.colorBy == '",nonNumCols,"'", collapse = "||"),
        uiOutput(ns("clusterColorPanel"))
      ),
      ## Item sidepanel
      selectInput(
        ns("background"),
        label = "Select Background",
        choices =c("Grey" = "grey", "White" = "white","Minimal" = "minimal", "Plain" =  "plain"),
        selected = "white"
      ),
      
      ## Item sidepanel
      sliderInput(
        ns("dotsize"), 
        "Choose a Dotsize",
        min = 0.01, 
        max = 2, 
        value = 1
      ),
      ## Item sidepanel
      checkboxInput(
        ns("showPlotLegend"), 
        "Show Plot Legends", 
        value = TRUE, 
        width = NULL
      ),
      
      ## Item sidepanel
      bookmarkButton(),
      br(),
      br(),
      ## Item sidepanel
      downloadButton(ns('plotDLall'), "Download Plot Images"),
      br(),
      br(),
      ## Item sidepanel
      conditionalPanel(
        condition = paste0('input[\'', ns('lg10Expr'), "\'] != \'lg10Expr\'"),
        downloadButton(ns("downloadData"), "Download Color Selection")
      ),
      uiOutput(ns("clusterColorPanel"))
    ), # End sidepanel
    mainPanel(
      plotOutput(ns("scApp"))
    )
  )
}
    
#' scFeatureView Server Functions
#'
#' @noRd 
mod_scFeatureView_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ################################
    ## Load gene list server side ##
    startUpList <- golem::get_golem_options(which = "startUpList")
    allGenes <- startUpList$utilityList$allGenes
    geneDefault <- startUpList$keyList$geneDefault
    
    updateSelectizeInput(
        session, 
        'gene', 
        choices = allGenes, 
        selected = geneDefault, 
        server = TRUE
    )
    
    ## Done loading gene list server side ##
    ########################################
    
    ########################################
    ## Create Dynamic Color Selectors     ##
    observe({
      startUpList <- golem::get_golem_options(which = "startUpList")
      numCols <- startUpList$utilityList$numCols
      print(input$colorBy)
      if (!(input$colorBy %in% numCols)){
        dfColorTable <-  createColorTable(startUpList = startUpList, colorBy = input$colorBy)
        print(dfColorTable)
        
        nameCol <- names(dfColorTable)[1]
        nameColCol <- "dotColor"
        labelID <- names(dfColorTable)[1]
        
        
        dfColSel <- dfColorTable
        colVec <- as.vector(dfColSel[,nameColCol])
        names(colVec) <- as.vector(dfColSel[,nameCol])
        colVec <- colVec[colVec != ""]
        dfColSel <- dfColSel[order(dfColSel[,nameCol]),]
        
        output$clusterColorPanel = renderUI({
          dfColSel[["label"]] <- paste0(dfColSel[,nameCol], " ", labelID," Color" )
          input_list <- lapply(1:nrow(dfColSel), function(i) {
            # for each dynamically generated input, give a different name
            clusterName <- as.vector(dfColSel[i,nameCol])
            clusterColor <- as.vector(dfColSel[i,nameColCol])
            label <- paste0(as.vector(dfColSel[i,nameCol]), " ",labelID," Color")
            colourpicker::colourInput(inputId = ns(clusterName), label = label, value = clusterColor)
          })
          
          do.call(tagList, input_list)
        })
      }
      
    })
    
    output$scApp1 <- renderPlot({
      
      data <- get_data(
        table = "pcaTB",
        colSel = c("sample_id", "sample_group", input$x_axis, input$y_axis)
      )
      
      cols <- paste0("c(", paste0("input$col", as.vector(data[,input$colorBy]), collapse = ", "), ")")
      cols <- eval(parse(text = cols))
      
      
      # To prevent errors
      req(length(cols) == length(as.vector(data[,input$colorBy])))
      print(cols)
      
      #cols <- eval(parse(text = cols))
      
      #req(length(cols) == length(unique(data[,input$colorBy])))
      
      p1 <- plotPcaScatter(data = data, x_axis = input$x_axis, y_axis = input$y_axis, colorBy = input$colorBy, colors = cols)
      print(p1)
    })
    
    
  })
}
    
## To be copied in the UI
# mod_scFeatureView_ui("scFeatureView_1")
    
## To be copied in the server
# mod_scFeatureView_server("scFeatureView_1")
