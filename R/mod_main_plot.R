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
#' @importFrom shinydashboard box
mod_main_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(title = "Physical data", status = "primary", solidHeader = TRUE, collapsible = TRUE,
        plotlyOutput(ns("plot_vertical_phys_profile"))),
    box(title = "Biogeochemical data", status = "primary", solidHeader = TRUE, collapsible = FALSE,
      plotlyOutput(ns("plot_vertical_bio_profile"), height = "1000px"))
  )
}

#' main_plot Server Functions
#'
#' @noRd
mod_main_plot_server <- function(id, user_float_cycle){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # declare BGC-ARGO lists of core parameters (and beam attenuation..)
    bgc_params <- c('DOXY', 'CHLA', 'BBP700', 'TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION660', 'PH_IN_SITU_FREE', 'NITRATE', 'CDOM',
                    'DOWN_IRRADIANCE380', 'DOWN_IRRADIANCE412', 'DOWN_IRRADIANCE490', 'DOWNWELLING_PAR')

    ######################
    # TEMP and PSAL data #
    ######################

    # phys data to plot
    data_phys <- reactive({

      # fetch user input
      wmo <- user_float_cycle$wmo()
      cycle <- user_float_cycle$cycle()

      # wait for available wmo and cycle number (happens)
      shiny::validate(shiny::need(wmo > 0, message = "Loading... Please wait."))

      # format cycle number
      cycle <- dplyr::if_else(as.numeric(cycle) < 10, paste0('00',cycle), dplyr::if_else(as.numeric(cycle) < 100, paste0('0',cycle), cycle))

      # extract data from NetCDF
      # check if NetCDF exists in raw format
      ncfile <- paste0('/data1/GDAC/GDAC/coriolis/',wmo,'/profiles/R',wmo,'_',cycle,'.nc')
      if (file.exists(ncfile)) {
        extract_TS(ncfile)
      }else{ # data have been quality controlled in delayed mode
        extract_TS(ncfile = paste0('/data1/GDAC/GDAC/coriolis/',wmo,'/profiles/D',wmo,'_',cycle,'.nc'))
      }
    })

    # plot phys data
    output$plot_vertical_phys_profile <- renderPlotly({
      p <- purrr::map(c('TEMP', 'PSAL'), make_marker_plot, tb = data_phys())
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

      # format cycle number
      cycle <- dplyr::if_else(as.numeric(cycle) < 10, paste0('00',cycle), dplyr::if_else(as.numeric(cycle) < 100, paste0('0',cycle), cycle))

      # extract data from NetCDF for all selected variables
      # check if NetCDF exists in raw format
      #print(paste0('/data1/GDAC/GDAC/coriolis/',wmo,'/profiles/'))
      if(length(list.files(paste0('/data1/GDAC/GDAC/coriolis/',wmo,'/profiles/'), pattern = 'B')) == 0){ # no bio-optical data
        NULL
      }else if(file.exists(paste0('/data1/GDAC/GDAC/coriolis/',wmo,'/profiles/BR',wmo,'_',cycle,'.nc'))){ # check if data exists in raw (non delayed mode) format
        extract_BGC_parameters(ncfile = paste0('/data1/GDAC/GDAC/coriolis/',wmo,'/profiles/BR',wmo,'_',cycle,'.nc'))
      }else{ # data have been quality controlled in delayed mode
        extract_BGC_parameters(ncfile = paste0('/data1/GDAC/GDAC/coriolis/',wmo,'/profiles/BD',wmo,'_',cycle,'.nc'))
      }
    })

    # list of plotly plot based on input parameters
    plots_of_selected_parameters <- reactive({
      shiny::validate(
        need(nrow(data_bio()) > 0, message = 'No bio-optical data'))
      purrr::map(bgc_params, make_marker_plot, tb = data_bio(), wmo = user_float_cycle$wmo())
    })

    # plot bio data
    output$plot_vertical_bio_profile <- renderPlotly({
      req(plots_of_selected_parameters())
      #browser()
      plotly::subplot(plots_of_selected_parameters(), nrows = 2, shareY = T, titleX = T, margin = c(0.01,0,0.075,0)) %>% plotly::layout(showlegend=FALSE)
    })
  })
}
