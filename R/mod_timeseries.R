#' timeseries UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_timeseries_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(title = "Timeseries", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = "1000px",
        plotlyOutput(ns("plot_timeseries"), height = "900px")),
  )
}

#' timeseries Server Functions
#'
#' @noRd
mod_timeseries_server <- function(id, user_float_cycle){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # BGC-Argo (core and bgc) parameters
    bgc_params <- c('TEMP', 'PSAL', 'DOXY', 'CHLA', 'BBP700', 'TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION660', 'PH_IN_SITU_FREE', 'NITRATE', 'CDOM',
                    'DOWN_IRRADIANCE380', 'DOWN_IRRADIANCE412', 'DOWN_IRRADIANCE490', 'DOWNWELLING_PAR')

    # fetch user input
    wmo <- reactive(user_float_cycle$wmo())

    # extract data
    ts_data <- reactive({

      # wait for available wmo and cycle number (happens)
      shiny::validate(shiny::need(wmo() > 0, message = "Loading... Please wait."))

      # extract data from NetCDF
      # check if NetCDF exists in raw format
      ncfile <- paste0('/data1/GDAC/GDAC/coriolis/',wmo(),'/',wmo(),'_Rtraj.nc')
      if(file.exists(ncfile)){
        purrr::map_dfr(bgc_params, extract_timeseries_parameter, ncfile = ncfile)
      }else{ # no UVP6 particle data
        0
      }
    })

    # plot timeseries
    output$plot_timeseries <- plotly::renderPlotly({
      req(ts_data())
      shiny::validate(shiny::need(ts_data() != 0, message = 'No timeseries available.'))
      p <- purrr::map(bgc_params, make_timeseries_plot, tb = ts_data(), wmo = wmo())
      plotly::subplot(p, nrows = 1, shareY = T, shareX = T, titleX = T, margin = c(0.01, 0.01, 0.05, 0.15))

    })

  })
}

## To be copied in the UI
# mod_timeseries_ui("timeseries_1")

## To be copied in the server
# mod_timeseries_server("timeseries_1")
