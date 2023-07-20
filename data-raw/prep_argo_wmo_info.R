# # read bio index file (last download on 07/07/2023) to know which (usual) variable are available in a given float (note: not sure about parking data like UVP .. )
# bgc_index <- read.table('data-raw/argo_synthetic-profile_index.csv',skip=8,h=FALSE,sep=",")
#
# # rename header
# colnames(bgc_index) <- c('file', 'date', 'latitude', 'longitude', 'ocean', 'profiler_type', 'institution', 'parameters', 'parameter_data_mode', 'date_update')
#
# # clean data
# bgc_index <- bgc_index |>
#   dplyr::select(file, parameters, parameter_data_mode) |>
#   dplyr::mutate(wmo = purrr::map_chr(.x = bgc_index$file, .f = function(x) unlist(stringr::str_split(x, '/'))[2])) |>
#   dplyr::select(-file) |>
#   dplyr::distinct()
#
# usethis::use_data(bgc_index, overwrite = TRUE)


# ## same but not on the SYNTHETIC profile index, now it's on the BIO profile index..
# bio_index <- read.table('data-raw/argo_bio-profile_index.txt', skip = 8, h=TRUE, sep = ",")
#
# # extract wmo and clean a bit
# bio_index <- bio_index |>
#   dplyr::select(file, date, latitude, longitude, parameters, parameter_data_mode) |>
#   dplyr::mutate(wmo = purrr::map_chr(.x = bio_index$file, .f = function(x) unlist(stringr::str_split(x, '/'))[2])) |>
#   dplyr::distinct()
#
# usethis::use_data(bio_index, overwrite = TRUE)

# read bio index file to know which variables are available in a given float
bio_index <- read.table('/data1/GDAC/index_argo/argo_bio-profile_index.txt', skip = 8, h=TRUE, sep = ",")

# extract wmo and clean a bit
library(dplyr)
library(magrittr)
bio_index <- bio_index %>%
  dplyr::select(file, date, latitude, longitude, parameters, parameter_data_mode) %>%
  dplyr::mutate(wmo = purrr::map_chr(.x = bio_index$file, .f = function(x) unlist(stringr::str_split(x, '/'))[2])) %>%
  dplyr::distinct()

#usethis::use_data(bio_index, overwrite = TRUE)
# write to csv (not in a pqckage fashion using testhis but eh, those data are a mess)
vroom::vroom_write(bio_index, file = '/data-raw/bio_index.csv')
