#' @description Function that extracts UVP6 taxonomic data from BGC-Argo data (for vertical profiles)
#'
#' @param ncfile
#'
#' @return a tibble with 5 columns: value (number of objects), class (taxonomic class), depth, nb_image (number of UVP images) and taxo_abundance (taxonomic concentration in #/L)
#'
#' @export
#'
#' @noRd
#'
#' @example extract_uvp_taxo("/data1/GDAC/AUX/coriolis/4903634/profiles/R4903634_001_aux.nc")
#'

extract_uvp_taxo <- function(ncfile){

  # open NetCDF
  nc_data <- try(ncdf4::nc_open(ncfile))
  if(class(nc_data) == 'try-error'){
    return(0)
  }else{

    # extract number of objects in each taxo category
    number_objects_category <- ncdf4::ncvar_get(nc_data, 'NUMBER_OBJECTS_CATEGORY')

    if(class(number_objects_category)[1] != 'try-error'){ # there is no taxo data

      # find level (layer) index where the data is stored (don't know why this matrix has 2 empty layers but so far, this is my solution)
      level_index <- which(purrr::map_vec(.x = 1:3, .f = function(x) all(is.na(number_objects_category[,,x]))) == 0)
      number_objects_category <- number_objects_category[,,level_index]

      # transpose number of objects matrix
      number_objects_category <- tibble::as_tibble(t(number_objects_category))

      # extract index_category at the previously calculated level index
      index_category <- ncdf4::ncvar_get(nc_data, 'INDEX_CATEGORY')[,,level_index]
      index_category <- tibble::as_tibble(t(index_category)) # transpose matrix

      # find number of objects for each taxo categories
      taxo_abundance <- purrr::map_dfr(.x = c(0:19), .f = count_nb_objects, index_category = index_category, nb_objects = number_objects_category) # 0:19 because 20 classes in the UVP6 embedded classification at the moment

      # add depth
      pres <- ncdf4::ncvar_get(nc_data, 'PRES')[,level_index]
      taxo_abundance$depth <- rep.int(pres, 20) # 20 classes in the UVP6

      # extract number of images
      number_images_taxo <- ncdf4::ncvar_get(nc_data, 'NUMBER_IMAGES_PARTICLES_TAXO')[,level_index]
      taxo_abundance$nb_image <- rep.int(number_images_taxo, 20)

      # compute taxo concentration
      taxo_abundance <- taxo_abundance %>%
                            tidyr::drop_na(depth) %>%
                            dplyr::mutate(taxo_abundance = value/(0.7*nb_image)) # 0.7 = UVP6 image volume

      # close NetCDF
      ncdf4::nc_close(nc_data)

      return(taxo_abundance)
    }else{ # no particle data in the NetCDF
      return(0)
    }
  }
}

#' @description Function that finds the absolute position of a taxonomic category and returns the absolute number of organisms measured in that category
#'
#' @param index_category a tibble with the number of the taxonomic classes that have been measured/observed (a tibble of x rows and 40 columns)
#' @param class_number number of a taxo class (/!\ 0-indexed so class 0 = 'Acantharia') -> class max in the current UVP6 config = 19 (20 classes in total)
#' @param nb_objects a tibble with the number of objects for each category (a tibble of x rows and 40 columns)
#'
#' @return a one column tibble
#'
#' @noRd
#'
count_nb_objects <- function(class_number, index_category, nb_objects){

  # Find absolute position of selected taxo class using a binary mask
  object_position <- tibble::as_tibble(apply(index_category, c(1,2), function(x) any(x == class_number)))
  object_position <- replace(object_position, object_position == F, NA) # keep only positions where TRUE, replace by NA elsewhere

  # multiply TRUE boolean positions with the number of objects
  nb_objects <- nb_objects * object_position

  # resume numbers in a single array
  nb_objects <- rowSums(nb_objects, na.rm=T)

  # taxo categories
  taxo_cat <- c('Acantharia', 'Actinopterygii', 'Appendicularia', 'Aulacanthidae',
                'Calanoida', 'Chaetognatha', 'Collodaria', 'Creseis',
                'Foraminifera', 'Rhizaria', 'Salpida', 'Artefact',
                'Crystal', 'Detritus', 'Fiber<detritus', 'Other<living',
                'Puff', 'Small-bell<Hydrozoa', 'Solitaryglobule','Tuff')

  # make tibble
  taxo <- tibble::as_tibble(nb_objects)
  #colnames(taxo) <- taxo_cat[class_number+1]
  taxo$class <- taxo_cat[class_number+1]

  return(taxo)
}
