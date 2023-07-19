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
      tabPanel("Vertical profiles",
               #style = "overflow-y:scroll; max-height: 1000px; position:relative; align: centre",
               plotlyOutput(ns("plot_vertical_phys_profile"), width = "600px"),
               plotlyOutput(ns("plot_vertical_bio_profile"), width = "100%", height = "1000px")),
      #tabPanel("Timeseries")
    )
  )
}

#' main_plot Server Functions
#'
#' @noRd
mod_main_plot_server <- function(id, user_float_cycle){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # declare BGC-ARGO lists of core parameters (and beam attenuation..)
    bgc_params <- c('DOXY', 'CHLA', 'BBP700', 'CDOM', 'TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION660', 'NITRATE', 'PH_IN_SITU_FREE',
                    'DOWN_IRRADIANCE380', 'DOWN_IRRADIANCE412', 'DOWN_IRRADIANCE490', 'DOWNWELLING_PAR')

    ######################
    # TEMP and PSAL data #
    ######################

    # phys data to plot
    data_phys <- reactive({

      # fetch user input
      wmo <- user_float_cycle$wmo()
      cycle <- user_float_cycle$cycle()

      # format cycle number
      cycle <- dplyr::if_else(as.numeric(cycle) < 10, paste0('00',cycle), dplyr::if_else(as.numeric(cycle) < 100, paste0('0',cycle), cycle))

      # extract data from NetCDF
      extract_TS(ncfile = paste0('inst/extdata/',wmo,'/profiles/R',wmo,'_',cycle,'.nc'))
    })

    # plot phys data
    output$plot_vertical_phys_profile <- renderPlotly({
      req(data_phys())
      p <- purrr::map(c('TEMP', 'PSAL'), make_line_marker_plot, tb = data_phys())
      plotly::subplot(p, nrows = 1, shareY = T, titleX = T, margin = c(0.01, 0.01, 0.05, 0.15))
    })

    ######################
    # OTHER DATA ("bio") #
    ######################

    # bio data to plot (based on WMO, cycle number and parameter)
    data_bio <- reactive({

      # fetch user input
      wmo <- user_float_cycle$wmo()
      cycle <- user_float_cycle$cycle()
      param <- user_float_cycle$param()

      # format cycle number
      cycle <- dplyr::if_else(as.numeric(cycle) < 10, paste0('00',cycle), dplyr::if_else(as.numeric(cycle) < 100, paste0('0',cycle), cycle))

      # extract data from NetCDF for all selected variables
      dat <- extract_BGC_parameters(ncfile = paste0('inst/extdata/',wmo,'/profiles/BR',wmo,'_',cycle,'.nc'))

    })

    # list of plotly plot based on input parameters
    plots_of_selected_parameters <- reactive({
      shiny::validate(
        need(nrow(data_bio() > 0), message = "No bio-optical data"))
      purrr::map(bgc_params, make_line_marker_plot, tb = data_bio())
    })

    # plot bio data
    output$plot_vertical_bio_profile <- renderPlotly({
      req(plots_of_selected_parameters())
      plotly::subplot(plots_of_selected_parameters(), nrows = 2, shareY = T, titleX = T, margin = c(0.01,0,0.075,0))
    })
  })
}
