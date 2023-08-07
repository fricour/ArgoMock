#' helpers
#'
#' @description Function that extracts a variable (in ADJUSTED mode) if possible + its QC in a trajectory file
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
extract_timeseries_parameter <- function(ncfile, parameter){

  # open NetCDF
  nc_data <- try(ncdf4::nc_open(ncfile))
  if(class(nc_data) == 'try-error'){
    return(0)
  }else{
    # extract parameter
    value <- try(ncdf4::ncvar_get(nc_data, parameter))

    # check state of parameter
    if(class(value)[1] == 'try-error'){ # parameter does not exist, return empty tibble
      return(tibble::tibble(depth = NA, value = NA, qc = NA, juld = NA, parameter = !! parameter))
    }else{
      # check for adjusted values for that parameter
      value_adjusted <- try(ncdf4::ncvar_get(nc_data, paste0(parameter,'_ADJUSTED')))
      if(class(value_adjusted)[1] == 'try-error'){ # ADJUSTED field does not exist in NetCDF file for now
        depth <- ncdf4::ncvar_get(nc_data, 'PRES') # TODO : take the PRES ADJUSTED when possible?
        qc <- ncdf4::ncvar_get(nc_data, paste0(parameter,'_QC'))
        qc <- as.numeric(unlist(strsplit(qc,split="")))
        juld <- ncdf4::ncvar_get(nc_data, 'JULD') # TODO : take JULD ADJUSTED INSTEAD?
        juld <- oce::argoJuldToTime(juld)
        mc <- ncdf4::ncvar_get(nc_data, 'MEASUREMENT_CODE')
        tb <- tibble::tibble(depth = depth, value = value, qc = qc, juld, parameter = !! parameter, measurement_code = mc) %>% tidyr::drop_na(value, depth)
        return(tb)
      }else if(all(is.na(value_adjusted)) == TRUE){ # if TRUE, there are no adjusted values
        depth <- ncdf4::ncvar_get(nc_data, 'PRES') # TODO : take the PRES ADJUSTED when possible?
        qc <- ncdf4::ncvar_get(nc_data, paste0(parameter,'_QC'))
        qc <- as.numeric(unlist(strsplit(qc,split="")))
        juld <- ncdf4::ncvar_get(nc_data, 'JULD') # TODO : take JULD ADJUSTED INSTEAD?
        juld <- oce::argoJuldToTime(juld)
        mc <- ncdf4::ncvar_get(nc_data, 'MEASUREMENT_CODE')
        tb <- tibble::tibble(depth = depth, value = value, qc = qc, juld, parameter = !! parameter, measurement_code = mc) %>% tidyr::drop_na(value, depth)
        return(tb)
      }else{ # there are adjusted values
        depth <- ncdf4::ncvar_get(nc_data, 'PRES') # TODO : take the PRES ADJUSTED when possible?
        qc <- ncdf4::ncvar_get(nc_data, paste0(parameter,'_ADJUSTED_QC'))
        qc <- as.numeric(unlist(strsplit(qc,split="")))
        juld <- ncdf4::ncvar_get(nc_data, 'JULD') # TODO : take JULD ADJUSTED INSTEAD?
        juld <- oce::argoJuldToTime(juld)
        mc <- ncdf4::ncvar_get(nc_data, 'MEASUREMENT_CODE')
        tb <- tibble::tibble(depth = depth, value = value_adjusted, qc = qc, juld, parameter = !! parameter, measurement_code = mc) %>% tidyr::drop_na(value, depth)
        return(tb)
      }
    }
  }
}
