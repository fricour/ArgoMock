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
                choices = NULL
    ),
    selectizeInput(inputId = ns("cycle"),
                   label = "Cycle number",
                   choices = ""
    )
  )
}

#' select_float Server Functions
#'
#' @noRd
mod_select_float_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # use a server-side selectize, see at https://shiny.posit.co/r/articles/build/selectize/
    observe({
      updateSelectizeInput(session, 'wmo', choices = list.dirs("/data1/GDAC/GDAC/coriolis/", recursive = F, full.names = F), server = TRUE)
    })

    # list of ascending profiles
    observeEvent(input$wmo,{

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

