#' main_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput renderPlotly
mod_main_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Vertical profiles", plotlyOutput(ns("plot_vertical_profile"))),
      tabPanel("Timeseries")
    )
  )
}

#' main_plot Server Functions
#'
#' @noRd
mod_main_plot_server <- function(id, user_float_cycle){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #browser()

    # data to plot (based on WMO, cycle number and parameter)
    data <- reactive({

      # fetch user input
      wmo <- user_float_cycle$wmo()
      cycle <- user_float_cycle$cycle()
      param <- user_float_cycle$param()

      # format cycle number
      cycle <- dplyr::if_else(as.numeric(cycle) < 10, paste0('00',cycle), dplyr::if_else(as.numeric(cycle) < 100, paste0('0',cycle), cycle))

      # extract data from NetCDF
      tmp <- extract_one_field(ncfile = paste0('inst/extdata/',wmo,'/profiles/BR',wmo,'_',cycle,'.nc'), param)
    })

    output$plot_vertical_profile <- renderPlotly({
      make_line_marker_plot(data(), user_float_cycle$param())
    })
  })
}

# TO DO: Faire un minimum case, toujours afficher T, S + si possible les 6 cores variables (si elles existent) + le Cp et faire un
# menu déroulant. Laisser la possibilité d'aller checker d'autres variables? Pas obligé. + les données en log (note que je peux faire un tab complet en log si besoin)

