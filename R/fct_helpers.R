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

  # close NetCDF file
  ncdf4::nc_close(nc_data)

  # take the right column (same index as before)
  depth <- depth[,index_column]

  # make tibble
  tb <- tibble::tibble(depth = depth, value = selected_par, parameter = !!parameter)

  # rename parameter column
  #tb <- tb |> dplyr::rename(!!parameter := param)

  return(tb)
}

#' @description Function that extracts temperature and salinity data from the corresponding NetCDF
#'
#' @param ncfile path to a NetCDF file
#'
#' @return a dataframe with 3 columns: PRES (water depth), TEMP (temperature) and PSAL (salinity)
#'
#' @export
#'
#' @noRd
#'
#' @example
#' extract_TS(ncfile = 'inst/extdata/4903634/profiles/R4903634_001.nc')
#'
extract_TS <- function(ncfile){

  # open NetCDF file
  nc_data <- ncdf4::nc_open(ncfile)

  # make dataframe with temperature and salinity
  temp <- ncdf4::ncvar_get(nc_data, 'TEMP')
  psal <- ncdf4::ncvar_get(nc_data, 'PSAL')

  # determine active column (to get the good depth column based on the instrument that took the measurement)
  #index_column <- which(!is.na(psal[1,]))
  index_column <- 1 # FIXED
  temp <- temp[,index_column]
  psal <- psal[,index_column]

  # add pressure field
  depth <- ncdf4::ncvar_get(nc_data, 'PRES')

  # close NetCDF file
  ncdf4::nc_close(nc_data)

  # take the right column (same index as before)
  depth <- depth[,index_column]

  # make tibble
  tb <- tibble::tibble(depth = depth, TEMP = temp, PSAL = psal)
  tb <- tb |> tidyr::pivot_longer(cols = c(TEMP, PSAL), names_to = 'parameter')

  return(tb)
}

#' @description Function to plot oceanographic data using plotly
#'
#' @param tb a tibble with X columns (depth and selected parameters)
#' @param parameter_name name of selected parameter to plot (one parameter only here)
#'
#' @return a plotly plot
#'
#' @export
#'
#' @noRd
#'
#' @example
#  tmp <- extract_one_field(ncfile = 'inst/extdata/4903634/profiles/BR4903634_001.nc', 'DOXY')
#' make_line_marker_plot(tmp, 'DOXY')
make_line_marker_plot <- function(tb, parameter_name){

  # filter tb by parameter_name
  tmp <- tb |> dplyr::filter(parameter == parameter_name)

  # make plotly plot
  plotly::plot_ly(data = tmp, x = ~value, y = ~depth, type = 'scatter', mode = 'lines+markers',
  hovertemplate = paste("Depth: %{y:,.0f} m<br>", "x var: %{x:,.1f}", '<extra></extra>')) |>
  plotly::layout(xaxis = list(title = parameter_name), yaxis = list(title = 'Depth', autorange = "reversed")) |> plotly::toWebGL()

}

