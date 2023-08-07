#' auxiliary_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom shinydashboard box
mod_auxiliary_file_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(title = "UVP6 data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = "1000px",
        plotlyOutput(ns("plot_particle_profile"), height = "900px")),
  )
}

#' auxiliary_file Server Functions
#'
#' @noRd
mod_auxiliary_file_server <- function(id, user_float_cycle){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # UVP6 to plot
    particle_data <- reactive({

      # fetch user input
      wmo <- user_float_cycle$wmo()
      cycle <- user_float_cycle$cycle()

      # wait for available wmo and cycle number (happens)
      shiny::validate(shiny::need(wmo > 0, message = "Loading... Please wait."))

      # format cycle number
      cycle <- dplyr::if_else(as.numeric(cycle) < 10, paste0('00',cycle), dplyr::if_else(as.numeric(cycle) < 100, paste0('0',cycle), cycle))

      # extract data from NetCDF
      # check if NetCDF exists in raw format
      ncfile <- paste0('/data1/GDAC/AUX/coriolis/',wmo,'/profiles/R',wmo,'_',cycle,'_aux.nc')
      if (file.exists(ncfile)) {
        extract_LPM(ncfile)
      }else{ # no UVP6 particle data
        0
      }
    })

    # particle size classes for plotly plot
    lpm_classes <- c('NP_Size_50.8','NP_Size_64','NP_Size_80.6', 'NP_Size_102','NP_Size_128','NP_Size_161','NP_Size_203',
                     'NP_Size_256','NP_Size_323','NP_Size_406','NP_Size_512','NP_Size_645','NP_Size_813','NP_Size_1020','NP_Size_1290',
                     'NP_Size_1630','NP_Size_2050','NP_Size_2580')

    # render plotly plot
    output$plot_particle_profile <- renderPlotly({
      shiny::validate(shiny::need(particle_data() != 0, message = 'No UVP6 particle data.'))
      p <- purrr::map(lpm_classes, particle_plot, data = particle_data())
      plotly::subplot(p, nrows = 1, shareY = T, titleX = T, margin = c(0.01, 0.01, 0.05, 0.15))
    })

  })
}

## To be copied in the UI
# mod_auxiliary_file_ui("auxiliary_file_1")

## To be copied in the server
# mod_auxiliary_file_server("auxiliary_file_1")
