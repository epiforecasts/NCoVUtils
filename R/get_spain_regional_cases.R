#' Spain regional cases daily
#'
#' @description Extract regional spanish case counts.
#' [Source](https://covid19.isciii.es/)
#' @return A dataframe of daily Spanish regional case counts.
#' @importFrom readr read_csv
#' @importFrom dplyr select mutate filter left_join
#' @importFrom memoise cache_filesystem memoise
#' @importFrom lubridate dmy
#' @export
#' @examples
#'
#' \dontrun{
#'
#' # Regional map
#'
#' library(sf)
#' library(rnaturalearth)
#' library(ggplot2)
#'
#' data <- get_spain_regional_cases()
#'
#' regions <- rnaturalearth::ne_states("Spain", returnclass = "sf")
#'
#' regions_with_data <- regions %>%
#'   mutate(region_shortcode = stringr::str_remove_all(region_cod, "^ES\\."), # Match codes to dataset
#'          region_shortcode = stringr::str_replace(region_shortcode, "^PM$", "IB"), # Baleares
#'          region_shortcode = stringr::str_replace(region_shortcode, "^MU$", "MC"), # Murcia
#'          region_shortcode = stringr::str_replace(region_shortcode, "^NA$", "NC"), # Navarra
#'          region_shortcode = stringr::str_replace(region_shortcode, "^LO$", "RI"), # La Rioja
#'          region_shortcode = ifelse(region == "Melilla", "ME", region_shortcode) # Melilla
#'   ) %>%
#'   group_by(region_shortcode) %>%
#'   summarise(subregions = n(), do_union = TRUE) %>%
#'   left_join(data, by = c("region_shortcode" = "region"))
#'
#' # Map: Spain mainland + Baleares
#' es_mainland <- ggplot2::ggplot(regions_with_data) +
#'   geom_sf(ggplot2::aes(fill = cases)) +
#'   coord_sf(crs = st_crs(4326), xlim = c(-11, 4), ylim = c(35, 44))+
#'   theme_bw()
#'
#' # Map inset: Canarias
#' es_canarias <- dplyr::filter(regions_with_data, region == "Canarias")
#' es_canarias <- ggplot2::ggplot(es_canarias) +
#'   geom_sf(aes(fill = cases)) +  coord_sf(datum = NA) +
#'   xlab(es_canarias$region) +
#'   theme_bw() +  theme(legend.position = "none")
#'
#' # Map: mainland with insets
#' es_mainland +
#'   annotation_custom(
#'     grob = ggplotGrob(es_canarias),  xmin = -11,  xmax = -7,  ymin = 33.5,  ymax = 37.5)
#'
#'
#' }
#' ##Code
#' get_spain_regional_cases
get_spain_regional_cases <- function() {
  location <- "https://covid19.isciii.es/resources/serie_historica_acumulados.csv"
# Set up cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)
# Read data & calculate daily change in cases
  all_date <- suppressMessages(memoise::mem_read(location)) %>%
    dplyr::mutate(date = lubridate::dmy(Fecha)) %>%
    dplyr::select(region = 1, cases = 3, date)
# Work out daily cases from cumulative totals
  prev_date <- dplyr::filter(all_date, date == (max(date, na.rm=T)-1))
  data <- dplyr::filter(all_date, date == max(date, na.rm=T)) %>%
    dplyr::left_join(prev_date, by = c("region" = "region")) %>%
    dplyr::mutate(new_cases = cases.x - cases.y) %>%
    dplyr::select(region, new_cases)
  return(data)
}


