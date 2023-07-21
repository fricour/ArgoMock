#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  selected_float_cycle <- mod_select_float_server("sidebar")
  mod_main_plot_server("main_plot", selected_float_cycle)
  mod_auxiliary_file_server("aux_file", selected_float_cycle)
}
