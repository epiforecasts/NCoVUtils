#' Reset Cache and Update all Local Data
#' @param refresh_data Logical defaults to `FALSE`. Should all data sources be refreshed once the cache has been
#' removed.
#' @return Null
#' @export
#' @importFrom memoise cache_filesystem
#' @examples
#'
#'## Code
#'reset_cache
reset_cache <- function(refresh_data = FALSE) {

  unlink(".cache", recursive = TRUE)

  cache <- memoise::cache_filesystem(".cache")

  if (refresh_data) {
    
    tmp <- NCoVUtils::get_international_linelist()

    tmp <- NCoVUtils::get_who_cases()

    tmp <- NCoVUtils::get_italy_regional_cases()
    
    tmp <- NCoVUtils::get_ecdc_cases()
    
    tmp <- NCoVUtils::get_france_regional_cases()
    
    tmp <- NCoVUtils::get_germany_regional_cases()
    
    tmp <- NCoVUtils::get_interventions_data()
    
    tmp <- NCoVUtils::get_italy_regional_cases()
    
    tmp <- NCoVUtils::get_japan_regional_cases()
    
    tmp <- NCoVUtils::get_korea_regional_cases()
    
    tmp <- NCoVUtils::get_linelist()
    
    tmp <- NCoVUtils::get_spain_regional_cases()
    
    tmp <- NCoVUtils::get_total_cases()
    
    tmp <- NCoVUtils::get_uk_nhs_region_cases()
    
    tmp <- NCoVUtils::get_us_regional_cases()
    
  }

  return(invisible(NULL))
}

