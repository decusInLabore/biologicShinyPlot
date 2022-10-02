#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    # Create navbar
    shiny::navbarPage(
        title = "biologic SC",
        mod_biologicHeatmap_ui("biologicHeatmap_1", title = "Heatmap"),
        mod_biologicHeatmap_ui("biologicHeatmap_2", title = "Heatmap2"),
        mod_printTable_ui("printTable_1", title = "Table1"),
        mod_biologicScatter_ui("biologicScatter_1", title = "Scatter"),
        mod_dev_ui("dev_1", title = "Dev")
        
            
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "biologicHeatmap"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
