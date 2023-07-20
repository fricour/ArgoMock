#' helpers
#'
#' @description Function that extracts a variable (in ADJUSTED mode) if possible + its QC
#'
#' @param ncdata an open NetCDF file
#' @param parameter a BGC-ARGO parameter
#'
#' @return a tibble with 4 columns (depth, value of parameter (adjusted if possible), quality control and name of parameter)
#'
#' @export
#'
#' @noRd
#'
#' @example extract_parameter(nc_data, 'BBP700')
#'
extract_parameter <- function(nc_data, parameter){

  # extract parameter
  value <- try(ncdf4::ncvar_get(nc_data, parameter))

  # check state of parameter
  if(class(value)[1] == 'try-error'){ # parameter does not exist, return empty tibble
    return(tibble::tibble(depth = NA, value = NA, qc = NA, parameter = !! parameter))
  }else if(is.na(ncol(value))){ # parameter exists in a single column
    depth <- ncdf4::ncvar_get(nc_data, 'PRES') # TODO : take the PRES ADJUSTED when possible?

  }else{ # [CASE 1] parameter exists in multiple columns but we take the 'longest' one (the small column is associated with very shallow depths an usually less than 5 values)
         # [CASE 2] there are just simply one column related to the selected parameter among other columns filled with NA
    # find the good column to extract the data from
    index_column <- which(!is.na(value)[1,]) # if CASE 2
    if(length(index_column) > 1){index_column <- 1} # if CASE 1, 1 is FIXED (longest column seems to always be the first one)

    # get value based on column index
    value <- value[, index_column]

    # check for adjusted values for that parameter
    value_adjusted <- try(ncdf4::ncvar_get(nc_data, paste0(parameter,'_ADJUSTED')))
    if(class(value_adjusted)[1] == 'try-error'){ # ADJUSTED field does not exist in NetCDF file for now
      depth <- ncdf4::ncvar_get(nc_data, 'PRES') # TODO : take the PRES ADJUSTED when possible?
      depth <- depth[,index_column]
      qc <- ncdf4::ncvar_get(nc_data, paste0(parameter,'_QC'))
      qc <- as.numeric(unlist(strsplit(qc[index_column],split="")))
      return(tibble::tibble(depth = depth, value = value, qc = qc, parameter = !! parameter))
    }else if(all(is.na(value_adjusted[,1])) == TRUE){ # if TRUE, there are no adjusted values
      depth <- ncdf4::ncvar_get(nc_data, 'PRES') # TODO : take the PRES ADJUSTED when possible?
      depth <- depth[,index_column]
      qc <- ncdf4::ncvar_get(nc_data, paste0(parameter,'_QC'))
      qc <- as.numeric(unlist(strsplit(qc[index_column],split="")))
      return(tibble::tibble(depth = depth, value = value, qc = qc, parameter = !! parameter))
    }else{ # there are adjusted values
      depth <- ncdf4::ncvar_get(nc_data, 'PRES') # TODO : take the PRES ADJUSTED when possible?
      depth <- depth[,1]
      qc <- ncdf4::ncvar_get(nc_data, paste0(parameter,'_ADJUSTED_QC'))
      qc <- as.numeric(unlist(strsplit(qc[index_column],split="")))
      return(tibble::tibble(depth = depth, value = value_adjusted[,index_column], qc = qc, parameter = !! parameter))
    }
  }
}
#'
#' @description Function that extracts temperature and salinity data (if available) from a NetCDF
#'
#' @param ncfile path to a NetCDF file
#'
#' @return a tibble (4 columns: depth, value, qc and parameter)
#'
#' @export
#'
#' @noRd
#'
#' @example
#' extract_TS(ncfile = '/data1/GDAC/GDAC/coriolis/3902500/profiles/R3902500_001.nc')
extract_TS <- function(ncfile){

  # open NetCDF file
  nc_data <- ncdf4::nc_open(ncfile)

  # extract temperature and salinity data
  TS <- purrr::map_dfr(c('TEMP','PSAL'), .f = extract_parameter, nc_data = nc_data)

  # close NetCDF file
  ncdf4::nc_close(nc_data)

  return(TS)
}

#' @description Function that extracts optical data from NetCDF
#'
#' @param ncfile path to a NetCDF file
#'
#' @return a tibble (4 columns: depth, value, qc and parameter)
#'
#' @export
#'
#' @noRd
#'
#' @example
#' extract_BGC_parameters(ncfile = '/data1/GDAC/GDAC/coriolis/3902500/profiles/BR3902500_001.nc')
extract_BGC_parameters <- function(ncfile){

  # List of BGC parameters (to be completed..)
  bgc_params <- c('DOXY', 'CHLA', 'BBP700', 'TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION660', 'PH_IN_SITU_FREE', 'NITRATE', 'CDOM',
                  'DOWN_IRRADIANCE380', 'DOWN_IRRADIANCE412', 'DOWN_IRRADIANCE490', 'DOWNWELLING_PAR')

  # open NetCDF file
  nc_data <- ncdf4::nc_open(ncfile)

  # extract variables from NetCDF
  tb <- purrr::map_dfr(bgc_params, extract_parameter, nc_data = nc_data)

  # close NetCDF file
  ncdf4::nc_close(nc_data)

  return(tb)
}

#' @description Function that returns attributes (e.g. log scale, number of digits, etc) for a plotly plot
#'
#' @param parameter A bgc parameter
#'
#' @return list of specifics
#'
#' @export
#'
#' @noRd
#'
get_plot_attributes <- function(parameter){

  # list of bgc parameters + temperature and salinity
  list_of_params <- c('DOXY', 'CHLA', 'BBP700', 'TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION660', 'PH_IN_SITU_FREE', 'NITRATE', 'CDOM',
                  'DOWN_IRRADIANCE380', 'DOWN_IRRADIANCE412', 'DOWN_IRRADIANCE490', 'DOWNWELLING_PAR')

  if(parameter == 'TEMP'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f} °C", type = "-"))
  }else if(parameter == 'PSAL'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f} PSU", type = "-"))
  }else if(parameter == 'DOXY'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f} &mu;mol/kg", type = "-"))
  }else if(parameter == 'CHLA'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f} mg/m<sup>3</sup>", type = "-"))
  }else if(parameter == 'BBP700'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1e} m<sup>-1</sup>", type = "-"))
  }else if(parameter == 'TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION660'){
    return(list(parameter_name = 'CP600', xdigits = "%{x:,.2f} m<sup>-1</sup>", type = "-"))
  }else if(parameter == 'PH_IN_SITU_FREE'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f}", type = "-"))
  }else if(parameter == 'NITRATE'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f} &mu;mol/kg", type = "-"))
  }else if(parameter == 'CDOM'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f} ppb", type = "-"))
  }else if(parameter == 'DOWN_IRRADIANCE380'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1e} W m<sup>-2</sup> nm<sup>-1</sup>", type = "log"))
  }else if(parameter == 'DOWN_IRRADIANCE412'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1e} W m<sup>-2</sup> nm<sup>-1</sup>", type = "log"))
  }else if(parameter == 'DOWN_IRRADIANCE490'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1e} W m<sup>-2</sup> nm<sup>-1</sup>", type = "log"))
  }else{
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f} &mu;molQuanta m<sup>-2</sup> sec <sup>-1</sup>", type = "log"))
  }
}

#' @description Function to plot oceanographic data using plotly
#'
#' @param tb a tibble with 4 columns: depth, value, qc and parameter name
#' @param parameter_name name of selected parameter to plot (one parameter only here)
#'
#' @return a plotly plot
#'
#' @export
#'
#' @noRd
#'
#' @example
#' tmp <- extract_BGC_parameters(ncfile = '/data1/GDAC/GDAC/coriolis/6904240/profiles/BR6904240_001.nc')
#' make_marker_plot(tmp, 'DOXY')
make_marker_plot <- function(tb, parameter_name, wmo){

  # filter tb by parameter_name
  tmp <- tb %>% dplyr::filter(parameter == parameter_name, !is.na(depth))

  # special case for the CP660 (at this time, values needs to be converted)
  if(parameter_name == 'TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION660'){
    CSCdark <- ArgoDownload::c_rover_calib[ArgoDownload::c_rover_calib$WMO == wmo,]$CSCdark
    CSCcal <- ArgoDownload::c_rover_calib[ArgoDownload::c_rover_calib$WMO == wmo,]$CSCcal
    x <- 0.25
    tmp$value <- -log((tmp$value - CSCdark)/(CSCcal-CSCdark))/x
  }

  # convert QC to strings for discrete colours
  tmp$qc <- as.character(tmp$qc)

  # create discrete colour palette
  pal <- c("yellow", '#48C9B0', '#5499C7', '#EB984E', '#E74C3C', '#ee3a8c', '#78747B')
  pal <- stats::setNames(pal, c("0", "1", "2", "3", "4", "5", "8"))

  # get plot attributes based on parameter_name
  plot_attributes <- get_plot_attributes(parameter_name)

  # make plotly plot
  tmp %>% dplyr::group_by(parameter) %>% plotly::plot_ly(x = ~value, y = ~depth, type = 'scatter', mode = 'markers',
                  text = ~qc,
                  color = ~qc,
                  colors = pal,
                  legendgroup = ~qc,
                  hovertemplate = paste(" DEPTH: %{y:,.0f} m<br>", paste0(plot_attributes[[1]],": ",plot_attributes[[2]],"<br>"), paste0('QC: %{text:, .0f}'))) %>%
    plotly::layout(xaxis = list(title = plot_attributes[[1]], type = plot_attributes[[3]]),
                   yaxis = list(title = 'DEPTH', autorange = "reversed"),
                   showlegend = FALSE) #%>% plotly::toWebGL()

}
