#' Get Russia daily cases
#'
#'
#' @description Fetches COVID case counts by region
#' This data is sourced from https://github.com/grwlf/COVID-19_plus_Russia#data-sources
#' @return A dataframe of case counts in Russian regions
#' @export
#' @importFrom memoise cache_filesystem memoise
#' @importFrom dplyr select group_by ungroup mutate left_join
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_csv
#' @importFrom lubridate mdy
#' @examples
#'
#'
#'\dontrun{
#'
#'

# # Mapping
# regions <- rnaturalearth::ne_states(geounit = "Russia", returnclass = "sf")
# data <- get_russia_regional_cases() %>%
#   dplyr::filter(date == max(date))
# regions_with_data <- dplyr::left_join(regions, data, by = c("iso_3166_2" = "iso_code"))
# regions_with_data %>%
#   ggplot2::ggplot(ggplot2::aes(fill = cases)) +
#   ggplot2::geom_sf() +
#   ggplot2::coord_sf(xlim = c(20, max(sf::st_coordinates(regions_with_data))))
#'   }

get_russia_regional_cases <- function() {

  # Read data
  path <- "https://raw.githubusercontent.com/grwlf/COVID-19_plus_Russia/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_RU.csv"

  # Using cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)
  russia <- mem_read(path)

  # Reshape
  russia <- russia %>%
    tidyr::pivot_longer(cols = 12:tidyr::last_col(), names_to = "date") %>%
    dplyr::select(date, country = Country_Region, region = Province_State, cases = value) %>%
    dplyr::mutate(date = lubridate::mdy(date))

  # Cumualative to daily
  russia <- dplyr::group_by(russia, region) %>%
    dplyr::mutate(
      cases = c(cases[1], diff(cases)),
      cases = ifelse(cases < 0, 0, cases)) %>%
    dplyr::ungroup()

  # Join to ISO region codes
  get_russia_iso_codes <- function() {
    region_url <- "https://en.wikipedia.org/wiki/ISO_3166-2:RU"
    iso_table <- region_url %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
      rvest::html_table(fill=TRUE)
    iso_code <- iso_table[[1]][-1,]$Code

    iso_codes <- tibble::tibble(
      iso_code = c(iso_code, "UA-40", "UA-43"),
      region = c("Adygea Republic", "Altai Republic", "Bashkortostan Republic", "Buryatia Republic",
                 "Chechen Republic", "Chuvashia Republic", "Dagestan Republic", "Ingushetia Republic",
                 "Kabardino-Balkarian Republic", "Kalmykia Republic", "Karachay-Cherkess Republic", "Karelia Republic",
                 "Khakassia Republic", "Komi Republic", "Mari El Republic", "Mordovia Republic",
                 "Sakha (Yakutiya) Republic", "North Ossetia - Alania Republic", "Tatarstan Republic",
                 "Tyva Republic",  "Udmurt Republic",  "Altai Krai", "Kamchatka Krai",
                 "Khabarovsk Krai",  "Krasnodar Krai", "Krasnoyarsk Krai", "Perm Krai", "Primorsky Krai", "Stavropol Krai",
                 "Zabaykalsky Krai", "Amur Oblast", "Arkhangelsk Oblast", "Astrakhan Oblast", "Belgorod Oblast",
                 "Bryansk Oblast", "Chelyabinsk Oblast",  "Irkutsk Oblast", "Ivanovo Oblast",
                 "Kaliningrad Oblast", "Kaluga Oblast", "Kemerovo Oblast", "Kirov Oblast",
                 "Kostroma Oblast", "Kurgan Oblast", "Kursk Oblast", "Leningrad Oblast",
                 "Lipetsk Oblast", "Magadan Oblast", "Moscow Oblast", "Murmansk Oblast",
                 "Nizhny Novgorod Oblast", "Novgorod Oblast", "Novosibirsk Oblast", "Omsk Oblast",
                 "Orenburg Oblast",  "Orel Oblast",  "Penza Oblast", "Pskov Oblast",
                 "Rostov Oblast", "Ryazan Oblast", "Sakhalin Oblast",  "Samara Oblast",
                 "Saratov Oblast", "Smolensk Oblast", "Sverdlovsk Oblast",  "Tambov Oblast",
                 "Tomsk Oblast", "Tula Oblast", "Tver Oblast",  "Tyumen Oblast",  "Ulyanovsk Oblast",
                 "Vladimir Oblast", "Volgograd Oblast",  "Vologda Oblast",  "Voronezh Oblast",
                 "Yaroslavl Oblast", "Moscow", "Saint Petersburg", "Jewish Autonomous Okrug", "Chukotka Autonomous Okrug",
                 "Khanty-Mansi Autonomous Okrug", "Nenets Autonomous Okrug", "Yamalo-Nenets Autonomous Okrug", "Sevastopol",
                 "Republic of Crimea"))

    return(iso_codes)
  }

  iso_codes <- get_russia_iso_codes()
  russia <- dplyr::left_join(russia, iso_codes, by = "region")

  return(russia)

}

