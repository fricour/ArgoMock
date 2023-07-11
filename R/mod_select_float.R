#' select_float UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_float_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("wmo"),
                label = "Float WMO",
                choices = list.dirs("inst/extdata/", full.names = FALSE, recursive = FALSE)
    ),
    selectizeInput(inputId = ns("cycle"),
                   label = "Cycle number",
                   choices = ""
    ),
    selectizeInput(inputId = ns("params"),
                   label = "Available parameters",
                   choices = "",
                   multiple = FALSE
    )
  )
}

#' select_float Server Functions
#'
#' @noRd
mod_select_float_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # list of parameters based on selected WMO
    observe({
      all_parameters <- unique(unlist(purrr::map(dplyr::filter(ArgoDownload::bio_index, wmo == input$wmo)$parameters, .f = function(x) stringr::str_split(x, pattern = ' '))))
      # add temperature and salinity to bio data
      all_parameters <- c("TEMP","PSAL", all_parameters)
      # remove MEDIAN and STD parameters (not needed here)
      all_parameters <- stringr::str_subset(all_parameters, "MED", negate = TRUE)
      all_parameters <- stringr::str_subset(all_parameters, "STD", negate = TRUE)
      updateSelectInput(session,
                        "params",
                        choices = all_parameters)
    })

    # list of ascending profiles
    observe({
      # keep only bio profiles
      ascending_cycles <- stringr::str_subset(list.files(paste0("inst/extdata/",input$wmo,"/profiles/")), pattern = "B")
      # remove descending profile(s)
      ascending_cycles <- stringr::str_subset(ascending_cycles, pattern = "D.nc", negate = TRUE)
      # update number of cycle based on selected float
      updateSelectizeInput(session,
                           "cycle",
                           choices = 1:length(ascending_cycles))
    })

    # return selected WMO and profile number
    list(
      wmo = reactive(input$wmo),
      cycle = reactive(input$cycle),
      param = reactive(input$params)
    )
  })
}

