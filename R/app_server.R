#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  selected_float_cycle <- mod_select_float_server("sidebar_1")
  #browser()
  mod_main_plot_server("main_plot_1", selected_float_cycle)
}
