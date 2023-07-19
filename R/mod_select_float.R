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
                   label = "Other parameters",
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
      other_parameters <- unique(unlist(purrr::map(dplyr::filter(ArgoDownload::bio_index, wmo == input$wmo)$parameters, .f = function(x) stringr::str_split(x, pattern = ' '))))

      # set bgc parameters (to be completed..)
      bgc_params <- c('DOXY', 'CHLA', 'BBP700', 'CDOM', 'TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION660', 'NITRATE', 'PH_IN_SITU_FREE',
                      'DOWN_IRRADIANCE380', 'DOWN_IRRADIANCE412', 'DOWN_IRRADIANCE490', 'DOWNWELLING_PAR')

      # remove bgc parameters
      other_parameters <- dplyr::setdiff(other_parameters, bgc_params)

      # remove pressure field (depth)
      all_parameters <- stringr::str_subset(other_parameters, "PRES", negate = TRUE)

      updateSelectInput(session,
                        "params",
                        choices = all_parameters,
                        selected = '')
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

