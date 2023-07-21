#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    # fluidPage(
    #   titlePanel("Visualize BGC-Argo data (from Coriolis)"),
    #   sidebarLayout(
    #     sidebarPanel = sidebarPanel(mod_select_float_ui("sidebar"),
    #                                 width = 2),
    #     mainPanel = mainPanel(
    #       tabsetPanel(
    #         tabPanel("Core profiles",
    #                  mod_main_plot_ui("main_plot")),
    #         tabPanel("Auxiliary profiles"))
    #     )
    #   )
    # )
    dashboardPage(
      dashboardHeader(
        title = 'Visualize BGC-Argo data (from Coriolis)'
      ),
      dashboardSidebar(
        mod_select_float_ui("sidebar")
      ),
      dashboardBody(
        tabsetPanel(
            tabPanel("Core profiles",
                    mod_main_plot_ui("main_plot")),
            tabPanel("Auxiliary profiles"))
        )
      )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ArgoDownload"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
