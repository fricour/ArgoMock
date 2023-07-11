#' helpers
#'
#' @description Function that extracts data from NetCDF
#'
#' @param ncfile path to a NetCDF file
#' @param parameter field to extract from the NetCDF
#'
#' @return a dataframe with 2 columns: PRES (water depth) and the field specified by the user
#'
#' @export
#'
#' @noRd
#'
#' @example
#' extract_one_field(ncfile = 'inst/extdata/4903634/profiles/BR4903634_001.nc', 'DOXY')
#'
extract_one_field <- function(ncfile, parameter){

  # open NetCDF file
  nc_data <- ncdf4::nc_open(ncfile)

  # make dataframe with selected parameter
  # extract parameter
  selected_par <- ncdf4::ncvar_get(nc_data, parameter)

  # determine active column (to get the good depth column based on the instrument that took the measurement)
  index_column <- which(!is.na(selected_par[1,]))
  selected_par <- selected_par[,index_column]

  # add pressure field
  depth <- ncdf4::ncvar_get(nc_data, 'PRES')

  # take the right column (same index as before)
  depth <- depth[,index_column]

  # make tibble
  tb <- tibble::tibble(depth = depth, parameter = selected_par)
  return(tb)
}

#' @description Function to plot oceanographic data using plotly
#'
#' @param tb a tibble with 2 columns (depth and selected parameter)
#' @param x x variable to plot
#' @param y y variable to plot
#'
#' @return a plotly plot
#'
#' @export
#'
#' @noRd
#'
#' @example
#  tmp <- extract_one_field(ncfile = 'inst/extdata/4903634/profiles/BR4903634_001.nc', 'DOXY')
#' make_line_marker_plot(tmp, 'test')
make_line_marker_plot <- function(tb, parameter_name, x = 'parameter', y = 'depth'){

  plotly::plot_ly(data = tb, x = ~get(x), y = ~get(y), type = 'scatter', mode = 'lines+markers',
  hovertemplate = paste("Depth: %{y:,.0f} m<br>", "x var: %{x:,.1f}", '<extra></extra>')) |>
  plotly::layout(xaxis = list(title = parameter_name), yaxis = list(title = 'Depth', autorange = "reversed")) |> plotly::toWebGL()

}
