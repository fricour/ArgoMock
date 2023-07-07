#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_main_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Float info table",
               DT::dataTableOutput(ns("tab"))),
      tabPanel("Example with float 4903634 ")

    )
  )
}

#' main Server Functions
#'
#' @noRd
mod_main_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # show table at all times (no need for a module then...)
    output$tab <- DT::renderDataTable(ArgoDownload::bgc_index)

  })
}

## To be copied in the UI
# mod_main_ui("main_1")

## To be copied in the server
# mod_main_server("main_1")
