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
               plotlyOutput(ns("plot_vertical_phys_profile"), width = 8),
               plotlyOutput(ns("plot_vertical_bio_profile"), height = "1000px")),
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
      print(head(data_phys()))
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
      purrr::map_dfr(user_float_cycle$param(), extract_one_field, ncfile = paste0('inst/extdata/',wmo,'/profiles/BR',wmo,'_',cycle,'.nc'))

    })

    # list of plotly plot based on input parameters
    plots_of_selected_parameters <- reactive({
      req(data_bio())
      purrr::map(user_float_cycle$param(), make_line_marker_plot, tb = data_bio())
    })

    # plot bio data
    output$plot_vertical_bio_profile <- renderPlotly({
      req(plots_of_selected_parameters())
      #browser()
      l_array <- length(plots_of_selected_parameters())
      if(l_array <= 5){nrow <- 1}else{nrow <- floor(l_array/5) + 1}
      plotly::subplot(plots_of_selected_parameters(), nrows = nrow, shareY = T, titleX = T, margin = c(0.01, 0.01, 0.05, 0.15))
    })
  })
}
