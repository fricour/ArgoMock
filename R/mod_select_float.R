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
                choices =list.dirs("/data1/GDAC/GDAC/coriolis/", recursive = F, full.names = F)
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

    # read bio index
    bio_index <- vroom::vroom('/home/ricour/ArgoMock/data-raw/bio_index.csv')

    # list of parameters based on selected WMO
    observe({
      other_parameters <- unique(unlist(purrr::map(dplyr::filter(bio_index, wmo == input$wmo)$parameters, .f = function(x) stringr::str_split(x, pattern = ' '))))

      # set bgc parameters (to be completed..)
      bgc_params <- c('DOXY', 'CHLA', 'BBP700', 'TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION660', 'PH_IN_SITU_FREE', 'NITRATE', 'CDOM',
                      'DOWN_IRRADIANCE380', 'DOWN_IRRADIANCE412', 'DOWN_IRRADIANCE490', 'DOWNWELLING_PAR')

      # remove bgc parameters
      other_parameters <- dplyr::setdiff(other_parameters, bgc_params)

      # remove pressure field (depth)
      other_parameters <- stringr::str_subset(other_parameters, "PRES", negate = TRUE)

      updateSelectInput(session,
                        "params",
                        choices = other_parameters,
                        selected = '')
    })

    # list of ascending profiles
    observe({
      # count only ascending profiles (do not count profiles starting with B or S)
      ascending_cycles <- stringr::str_subset(list.files(paste0('/data1/GDAC/GDAC/coriolis/',input$wmo,'/profiles/')), pattern = "B", negate = TRUE)
      ascending_cycles <- stringr::str_subset(ascending_cycles, pattern = "S", negate = TRUE)

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

