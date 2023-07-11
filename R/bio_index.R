#' Argo bio profile index
#'
#'
#' @format ## `bgc_index`
#' A data frame with 288,359 rows and 7 columns:
#' \describe{
#'   \item{file}{Where the file is located on the GDACs}
#'   \item{date}{Profile date}
#'   \item{latitude}{Latitude}
#'   \item{longitude}{Longitude}
#'   \item{parameters}{Parameters measured by the float}
#'   \item{parameter_data_mode}{Data processing mode (R: Raw, A: Adjusted, D: Delayed)}
#'   \item{wmo}{World Meteorological Object}
#' }
#' @source <ftp://ftp.ifremer.fr/ifremer/argo/argo_bio-profile_index.txt>
"bio_index"
