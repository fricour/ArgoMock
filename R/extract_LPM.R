#' @description Function that extracts UVP6 particle data from a NetCDF
#'
#' @param ncfile
#'
#' @return a tibble with 18 columns
#'
#' @export
#'
#' @noRd
#'
#' @example extract_LPM_data("/data1/GDAC/AUX/coriolis/4903634/profiles/R4903634_001_aux.nc")
#'

extract_LPM <- function(ncfile){

  # open NetCDF
  nc_data <- try(ncdf4::nc_open(ncfile))
  if(class(nc_data) == 'try-error'){
    return(0)
  }else{
    # extract pressure field
    pres <- ncdf4::ncvar_get(nc_data, 'PRES')[,1] # FIXED because format has changed and NB_SIZE_SPECTRA_PARTICLES usually is only filled in the first layer of the 3D matrix

    # extract particle size spectra
    part_spectra <- try(ncdf4::ncvar_get(nc_data, 'NB_SIZE_SPECTRA_PARTICLES')[,,1])

    if(class(part_spectra)[1] != 'try-error'){

      # transpose part spectra matrix
      part_spectra <- tibble::as_tibble(t(part_spectra))
      # lpm classes
      lpm_classes <- c('NP_Size_50.8','NP_Size_64','NP_Size_80.6', 'NP_Size_102','NP_Size_128','NP_Size_161','NP_Size_203',
                       'NP_Size_256','NP_Size_323','NP_Size_406','NP_Size_512','NP_Size_645','NP_Size_813','NP_Size_1020','NP_Size_1290',
                       'NP_Size_1630','NP_Size_2050','NP_Size_2580')
      # rename columns
      colnames(part_spectra) <- lpm_classes

      # extract number of images
      image_number <- ncdf4::ncvar_get(nc_data, 'IMAGE_NUMBER_PARTICLES_LPM')[,1]

      # divide particle concentrations by number of images
      part_spectra <- part_spectra %>% dplyr::mutate(dplyr::across(NP_Size_50.8:NP_Size_2580, ~.x/(0.7*image_number))) # 0.7 = UVP6 image volume

      # add depth to part_spectra
      part_spectra$depth <- pres
      part_spectra <- part_spectra %>% dplyr::select(depth, dplyr::everything())

      # close NetCDF
      ncdf4::nc_close(nc_data)

      return(part_spectra)
    }else{ # no particle data in the NetCDF
      return(0)
    }
  }
}


