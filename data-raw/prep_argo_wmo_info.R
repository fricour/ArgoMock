# read bio index file (last download on 07/07/2023) to know which (usual) variable are available in a given float (note: not sure about parking data like UVP .. )
bgc_index <- read.table('data-raw/argo_synthetic-profile_index.csv',skip=8,h=FALSE,sep=",")

# rename header
colnames(bgc_index) <- c('file', 'date', 'latitude', 'longitude', 'ocean', 'profiler_type', 'institution', 'parameters', 'parameter_data_mode', 'date_update')

# clean data
bgc_index <- bgc_index |>
  dplyr::select(file, parameters, parameter_data_mode) |>
  dplyr::mutate(wmo = purrr::map_chr(.x = bgc_index$file, .f = function(x) unlist(stringr::str_split(x, '/'))[2])) |>
  dplyr::select(-file) |>
  dplyr::distinct()

usethis::use_data(bgc_index, overwrite = TRUE)
