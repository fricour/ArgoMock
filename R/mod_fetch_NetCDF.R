#' fetch_NetCDF UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fetch_NetCDF_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' fetch_NetCDF Server Functions
#'
#' @noRd
mod_fetch_NetCDF_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_fetch_NetCDF_ui("fetch_NetCDF_1")

## To be copied in the server
# mod_fetch_NetCDF_server("fetch_NetCDF_1")
