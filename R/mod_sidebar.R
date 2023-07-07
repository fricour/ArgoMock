#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("wmo"),
      label = "Choose a WMO",
      choices = unique(ArgoDownload::bgc_index$wmo)
    ),
    selectInput(
      inputId = ns("params"),
      label = "Parameter(s) available",
      choices = "",
      multiple = TRUE
    )
  )
}

#' sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # list of parameters based on selected WMO
    observe({
      all_parameters <- unique(unlist(purrr::map(dplyr::filter(ArgoDownload::bgc_index, wmo == input$wmo)$parameters, .f = function(x) stringr::str_split(x, pattern = ' '))))
      updateSelectInput(session,
                      "params",
                      choices = all_parameters)
    })
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")
